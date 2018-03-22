(defun insert-one (e)
  "Insert an entity into the DB. NIL slots amount to NULL, or the default column
  value. NIL as an auto-increment field for example will generate a new
  auto-increment ID.

  Returns the value of LAST_INSERT_ID(). This will be the previous value of
  last_insert_id() if the ID is specified in the entity (i.e. the Autoincrement
  value wasn't used), or the value of the ID if autoincrement is used.

  Usage:

  ;; Define entity called test-entity...
  (defentity test-entity
      ((id \"BIGINT UNSIGNED\" :primary :auto-increment)
       (email \"VARCHAR(1024)\" :not-null)) T)
  ;; Insert an entity. The email of the entity will be \"a@a.a\", and the ID
  ;; will be auto-generated.
  (insert-one (make-entity :email \"a@a.a\"))"

  ;; First, get all the entity's slots as SQL column names. Skip nil values
  (let ((slots (remove-if (lambda (a)  (eq (slot-value e a) nil))
                          (mapcar #'sb-mop:slot-definition-name
                                  (sb-mop:class-direct-slots (class-of e))))))
    (let ((columns (mapcar (lambda (s) (kebab-to-snake-case (string s))) slots))
          (table-name (kebab-to-snake-case (string (type-of e)))))
      ;; Now build the query & parameter list
      (let ((query (format nil "INSERT INTO ~a (~{~a~^,~}) VALUES(~:*~{~*?~^,~})"
                           table-name columns))
            (params (loop for c in slots collect
                         (format nil "~a" (slot-value e c)))))
        ;; Actually query
        (apply #'dbi:execute (append (list (dbi:prepare *db* query)) params))
        ;; Get the last insert ID
        (nth 1 (dbi:fetch (dbi:execute
                           (dbi:prepare *db* "SELECT LAST_INSERT_ID()"))))))))
