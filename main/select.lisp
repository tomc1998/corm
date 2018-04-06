(in-package :corm)

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

(defun build-join-list-from-visit-list (tree)
  "Given a select tree, create a string containing left joins for all the items
in the tree, other than the root element"
  (let ((curr-list (list tree)) (str "") (p 1))
    (loop while (> (length curr-list) 0)
       do (let ((parent-p (- p 1)) (curr-item (pop curr-list)))
            (loop for n in (cadr curr-item) do
                 (push n curr-list)
               ;; Get this table's parent & child names
                 (let* ((parent-name (kebab-to-snake-case (string (car curr-item))))
                        (child-name (kebab-to-snake-case (string (car n))))
                        ;; Create left join text with big ugly format, this works i promise
                        (join (format nil "LEFT JOIN ~a AS ~a_~a ON ~a_~a.parent_~a_id = ~a_~a.id"
                                      child-name p child-name
                                      p child-name parent-name parent-p parent-name)))
                   (setf str (format nil "~a~%~a" str join)))
                 (setf p (+ p 1)))))
    str))

(defun add-to-parents (e-list e-node)
  "Add the given entity e to all the entities in the list e-list where e is the
child of the entity in e-list. The e-list is a list of entity nodes, (i.e. an
entity in the car, 2nd item is a list of children)"
  (let* ((e (car e-node))
         (slots (mapcar #'sb-mop:slot-definition-name
                        (sb-mop:class-direct-slots (class-of e))))
         (parent-slots
          (remove-if-not (lambda (s)
                           (let ((s (string s))) (and
                                                  (> (length s) 10)
                                                  (string-equal "-ID" (subseq s (- (length s) 3)))
                                                  (string-equal "PARENT-" (subseq s 0 7)))))
                         slots))
         ;; Names of the parents (just strip the 'parent-' and the '-id' from
         ;; the start and end of the string)
         (parent-names (mapcar (lambda (s)
                                 (subseq (string s) 7 (- (length (string s)) 3)))
                               parent-slots)))
    (loop for s in parent-slots for n in parent-names do
         (let* ((parent-id (slot-value e s))
                (parent (find-if
                         (lambda (_e)
                           (and
                            (string-equal (type-of (car _e)) n)
                            (= parent-id (slot-value (car _e) 'id))))
                         e-list)))
           (if parent (push e-node (cadr parent)))))))

(defun generate-where-clause (visit-list where)
  "Given a visit list and the where clause, generate a SQL where clause"
  (if (not (listp where)) (return-from generate-where-clause (kebab-to-snake-case (string where))))
  (cond
    ;; Process unary operations
    ((member (car where) '(not))
     (format nil "~a ~a"
             (string (car where))
             (generate-where-clause visit-list (second where))))
    ;; Process binary operations
    ((member (car where) '(= and or is like > >= < <= != sounds-like rlike - + * /))
     (format nil "~a ~a ~a"
             (generate-where-clause visit-list (second where))
             (string (car where))
             (generate-where-clause visit-list (third where))))
    ;; Assume we're going for a 'dotted access (i.e. 'user.id')'
    (t (kebab-to-snake-case (format nil "~a_~a" (position (car where) visit-list) (second where))))
    ))

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
  and another user with no posts."

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
         (sql (format nil "SELECT ~a FROM ~a AS 0_~:*~a ~a" column-spec
                      table-name
                      join-list))
         (rows (dbi:fetch-all (dbi:execute (dbi:prepare *db* sql))))
         ;; Create empty list which will contain all the entities
         (e-list ()))
    (loop for row in rows do
         (loop for e in visit-list for p from 0 do
              (let ((e (entity-from-row (make-instance e) p row)))
                ;; Make sure this entity is value (i.e. non-nil ID) and isn't a duplicate
                (if (and
                     (slot-value e 'id) ; Check ID non null
                     (not (member-if ; Check for duplicate
                           (lambda (_e)
                             (and
                              ;; Make sure they're the right type
                              (eq (type-of e) (type-of (car _e)))
                              ;; Compare IDs - if they're the same, we found a dupe
                              (=  (slot-value e 'id)
                                  (slot-value (car _e) 'id))))
                           e-list)))
                    (let ((e-node (list e ())))
                      (add-to-parents e-list e-node)
                      (push e-node e-list))))))
    ;; Remove non-top-level entity nodes from e-list
    (remove-if-not
     (lambda (s)
       (string-equal (string (car visit-list))
                     (string (type-of (car s))))) e-list)))
