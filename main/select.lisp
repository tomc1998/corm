(in-package :corm)

(define-condition entity-not-found-error (error) ())

(defun entity-from-row (e p r)
  "Given an entity, prefix, and row, extract the data from the row with
the given prefix and inserts the data into the entity of the given type. For convenience, returns e."
  (let* (
         ;; Get the slots of the entity
         (slots (mapcar #'sb-mop:slot-definition-name
                        (sb-mop:class-direct-slots (class-of e))))
         ;; Get slots as mysql column name symbols plus prefix (keywords)
         (column-names
          (mapcar (lambda (s)
                    (intern
                     (format nil "~a_~a" p
                             (kebab-to-snake-case (string s))) :keyword))
                  slots)))
    ;; loop over all the column names, and extract
    (loop for s in slots for c in column-names do
         (setf (slot-value e s) (getf r c))))
  e)

(defun build-visit-list-from-select-tree (tree)
  "Used by the select tree function to build a list of references to all the
nodes in the tree. Will build a breadth first traversal list."
  (let ((curr-list (list tree)) (final-list ()))
    (loop while (> (length curr-list) 0) do
         (let ((curr-item (pop curr-list)))
           (push (car curr-item) final-list)
           (loop for n in (cadr curr-item) do (push n curr-list))))
    (reverse final-list)))

(defun build-sql-column-spec-from-entity (e p)
  "Given an entity and a prefix, build a list for a SQL query to select the
entity's slots as columns"
  (let ((table-name (kebab-to-snake-case (string e)))
        (slots (mapcar (lambda (s) (kebab-to-snake-case
                                    (string (sb-mop:slot-definition-name s))))
                       (sb-mop:class-direct-slots (class-of (make-instance e))))))
    (format nil "~{~a~^, ~}"
            (loop for s in slots collect
                 (format nil "~a_~a.~a AS ~a_~a" p table-name s p s)))))

(defun build-join-from-entities (p0 p1 e0 e1)
  "Build join clause from 2 entities and prefixes. Takes into account m2m
joins."
  (let* ((e0-name (kebab-to-snake-case (string e0)))
         (e1-name (kebab-to-snake-case (string e1)))
         (is-m2m (member e1 (getf *m2m-meta* e0)))
         (m2m-table (getf (getf *m2m-meta* e0) e1)))
    ;; If this join is a m2m join, we need to generate a
    ;; more complex 'double' join to the intermediate table
    (if is-m2m
        (format nil
                (concatenate
                 'string
                 "LEFT JOIN ~a ON ~a.~a_id = ~a_~a.id "
                 "LEFT JOIN ~a AS ~a_~a ON ~a.~a_id = ~a_~a.id")
                m2m-table m2m-table e0-name p0 e0-name
                e1-name p1 e1-name m2m-table e1-name p1 e1-name)
        (format nil "LEFT JOIN ~a AS ~a_~a ON ~a_~a.parent_~a_id = ~a_~a.id"
                e1-name p1 e1-name
                p1 e1-name e0-name p0 e0-name))
    )
  )

(defun build-join-list-from-visit-list (tree)
  "Given a select tree, create a string containing left joins for all the items
in the tree, other than the root element"
  (let ((curr-list (list tree)) (str "") (p 1))
    (loop while (> (length curr-list) 0)
       do (let ((parent-p (- p 1)) (curr-item (pop curr-list)))
            (loop for n in (cadr curr-item) do
                 (push n curr-list)
                 (setf str
                       (format nil "~a~%~a" str
                               (build-join-from-entities
                                parent-p p (car curr-item) (car n))))
                 (setf p (+ p 1)))))
    str))

(defun generate-where-clause (visit-list where-expr)
  "Given a visit list and the where clause, generate a SQL where clause and a
    list of arguments. These are returned as a cons - the first item is a string
    (the sql query) and the cdr is the list of arguments."
  (let ((args ()))
    (labels ((inner (where)
               (if (not where) (error "NIL passed to where clause: ~s" where-expr))
               ;; If where isn't a list, assume this is an argument.
               (if (not (listp where))
                   (progn
                     (push where args)
                     (return-from inner "?")))
               (cond
                 ;; Process unary operations
                 ((member (car where) '(not))
                  (format nil "(~a ~a)"
                          (write-to-string (car where))
                          (inner (second where))))
                 ;; Process binary operations
                 ((member (string (car where))
                          '("=" "AND" "OR" "IS" "LIKE" ">" ">=" "<" "<=" "!="
                            "SOUNDS-LIKE" "RLIKE" "-" "+" "*" "/")
                          :test #'string=)
                  (format nil "(~a ~a ~a)"
                          (inner (second where))
                          (format nil "~a" (car where))
                          (inner (third where))))
                 ;; Assume we're going for a 'dotted access (i.e. 'user.id')'
                 (t (progn
                      (kebab-to-snake-case (format nil "~a_~a.~a" (position (car where) visit-list)
                                                  (car where) (second where))))))))
      ;; Create an inner function to allow recursive calls with over-arching state via closure
      (cons (inner where-expr) (reverse args)))))

(defun parse-tree-from-row (tree row e-list)
  "Parses a bunch of entities from the row using the given tree structure.
  Connects them up with parent entities in the e-list. Returns a new e-list with
  the new entities. Duplicates are removed (since a row is likely to contain an
  entity which was already parse)"
  (let ((queue `((nil (,tree)))))
    (loop while queue for p from 0 do
         ;; Loop over the children & try to parse
         (let ((parent (pop queue)))
           (loop for child in (second parent) when (not (null child)) do
                ;; Add this child to the queue (push to back)
                (if queue
                    (setf (cdr (last queue)) (list child))
                    (setf queue (list child)))
                ;; Now try to parse this entity from the row
                (let ((e (entity-from-row (make-instance (car child)) p row)))
                  ;; Make sure this entity is a value (i.e. non-nil ID) and isn't
                  ;; a duplicate
                  (if (and
                       (slot-value e 'id) ; Check ID non null
                       (not (member-if    ; Check for duplicate
                             (lambda (_e)
                               (and
                                ;; Make sure they're the right type
                                (eq (type-of e) (type-of (car _e)))
                                ;; Compare IDs - if they're the same, we found a
                                ;; dupe
                                (=  (slot-value e 'id)
                                    (slot-value (car _e) 'id))))
                             e-list)))
                      (let ((e-node (list e ()))
                            (parent-ix
                             (if (car parent)
                                 (position-if
                                  (lambda (e)
                                    (eq (car parent) (type-of (car e))))
                                  e-list))))
                        (if parent-ix
                            (setf (second (nth parent-ix e-list))
                                  (push e-node (second (nth parent-ix e-list)))))
                        (push e-node e-list)))))))
    e-list))

(defun select-tree (tree &key where)
  "Selects the tree of entities, and returns a tree in the same shape with the
  results. Each node of the input tree is the name of an entity, followed by a
  list of child nodes which are joined in a many to one relationship with the
  parent. For example, given the example of fetching all the posts made by a
  user, and all the comments on those posts:

  (select-tree '(user ((post ((comment ()))))) :where (= (user id) (post id))

  This would return a list of output nodes. Each output node is a list where the
  first element contains an entity instance, and the 2nd element is a list of
  child nodes. An example output for the above input would be:

  ((<user object>
    ((<post object>
      ((<comment object> ()))))
    ((<post object> ())))
  (<user object> ()))

  This represents 2 users, one who owns 2 posts (one of which has 1 comment),
  and another user with no posts.

  # Regarding many-to-many joins
  m2m joins are done automatically, given that an m2m join between the two
  tables has already been generated. For example,

  (def-many-to-many my-entity-1 my-entity-2)
  (select-tree '(my-entity-1 ((my-entity-2 ())))

  will perform the join accordingly to an intermediate table.
"

  ;; First, we need to generate the query string. We'll alias the column results
  ;; with a number based on their position in a deterministic traversal of the
  ;; tree. We can then traverse the tree, and use a generic function to parse
  ;; all the results out using the prefix as a namespace to make sure things
  ;; don't clash.
  (let* ((visit-list (build-visit-list-from-select-tree tree))
         (column-spec (format nil "~{~a~^, ~}"
                              (loop for v in visit-list for i from 0 collect
                                   (build-sql-column-spec-from-entity v i))))
         (join-list (build-join-list-from-visit-list tree))
         (table-name (kebab-to-snake-case (string (car tree))))
         (where-clause (if where (generate-where-clause visit-list where) '("TRUE")))
         (where-sql (car where-clause))
         (where-args (cdr where-clause))
         (sql (format nil "SELECT ~a FROM ~a AS 0_~:*~a ~a WHERE ~a" column-spec
                      table-name
                      join-list
                      where-sql))
         (rows (dbi:fetch-all (apply 'dbi:execute
                                     (append (list (dbi:prepare (get-conn) sql)) where-args))))
         ;; Create empty list which will contain all the entities
         (e-list ()))
    (loop for row in rows do
         (setf e-list (parse-tree-from-row tree row e-list)))
    ;; Remove non-top-level entity nodes from e-list
    (remove-if-not
     (lambda (s)
       (string-equal (string (car tree))
                     (string (type-of (car s))))) e-list)))

(defun check-owner-eq (e parent expected)
  "Convenience method to check the parent value of a given entity. Given an
  entity e (which has the slot 'id' bound properly), fetch the parent ID from
  the db and check whether it's equal to 'expected'. Returns t if it is, nil if
  it isn't, and raises 'entity-not-found-error' if not found. The 'parent'
  parameter should be the parent id symbol - for example, 'parent-user-id if you
  have an entity where you set the symbol 'user as the parent."
  (let ((tree (select-tree `(,(type-of e) ()) :where `(= ,(slot-value e 'id) (,(type-of e) id)))))
    (if (not tree) (error 'entity-not-found-error))
    (let ((fetched (caar tree)))
      (eq (slot-value fetched parent) expected))))
