(defmacro init-args-from-row (slots row)
  "From a list of slots & a mysql row, generate a list of initargs for an entity"
  `(loop
      for (k v) on ,row by #'cddr
      for s in ',slots
      append (list (intern (string s) :keyword) v)))

(defmacro select-all (e)
  "Select all entities of the given type. Usage:

  ;; Define entity called test-entity...
  (defentity test-entity
      ((id \"BIGINT UNSIGNED\" :primary :auto-increment)
       (email \"VARCHAR(1024)\" :not-null)) T)
  ;; Insert some entities
  (insert-one (make-entity :email \"a@mail.com\"))
  (insert-one (make-entity :email \"b@mail.com\"))
  (insert-one (make-entity :email \"c@mail.com\"))
  ;; Select all inserted test-entity entities, print their emails
  (loop for e in (select-all test-entity) do (print (slot-value e 'email)))"

  ;; Get slots of e & the table name
  (let ((slots (mapcar #'sb-mop:slot-definition-name
                       (sb-mop:class-direct-slots (class-of (make-instance e)))))
        (table-name (kebab-to-snake-case (string e))))
    ;; Get slots as column names
    (let ((columns (mapcar (lambda (s) (kebab-to-snake-case (string s))) slots)))
      ;; Build query
      (let ((query (format nil "SELECT ~{~a~^, ~} FROM ~a" columns table-name)))
        ;; Execute query & put into the entity
        `(loop for row in (dbi:fetch-all (dbi:execute (dbi:prepare *db* ,query)))
            ;; Make instance from initargs
            collect (apply #'make-instance
                           (append (list ',e) (init-args-from-row ,slots row))))))))

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
                 (format nil "~a.~a AS ~a_~a" table-name s p s)))))

(defun build-join-list-from-visit-list (tree)
  "Given a select tree, create a string containing left joins for all the items
in the tree, other than the root element"
  (let ((curr-list (list tree)) (str ""))
    (loop while (> (length curr-list) 0) for p from 0
       do (let ((curr-item (pop curr-list)))
            (loop for n in (cadr curr-item) for i from 1 do
                 (push n curr-list)
                 ;; Get this table's parent & child names
                 (let* ((parent-name (kebab-to-snake-case (string (car curr-item))))
                        (child-name (kebab-to-snake-case (string (car n))))
                        ;; Create left join text with big ugly format, this works i promise
                        (join (format nil " LEFT JOIN ~a AS ~a_~a ON ~a_~a.parent_~a_id = ~a_~a.id"
                                      child-name (+ p i) child-name
                                      (+ p i) child-name parent-name p parent-name)))
                   (setf str (format nil "~a~%~a" str join))))))
    str))

(defmacro select-tree (tree)
  "Selects the tree of entities, and returns a tree in the same shape with the
  results. Each node of the input tree is the name of an entity, followed by a
  list of child nodes which are joined in a many to one relationship with the
  parent. For example, given the example of fetching all the posts made by a
  user, and all the comments on those posts:

  (select-tree (user (post (comment ()))))

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
  )
