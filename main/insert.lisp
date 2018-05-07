(in-package :corm)

(define-condition insert-duplicate-error (error) ())

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
  (let ((db (get-conn))
        (slots (remove-if (lambda (a) (not (slot-boundp e a)))
                          (mapcar #'sb-mop:slot-definition-name
                                  (sb-mop:class-direct-slots (class-of e))))))
    (let ((columns (mapcar (lambda (s) (kebab-to-snake-case (string s))) slots))
          (table-name (kebab-to-snake-case (string (type-of e)))))
      ;; Now build the query & parameter list
      (let ((query (format nil "INSERT INTO ~a (~{~a~^,~}) VALUES(~:*~{~*?~^,~})"
                           table-name columns))
            (params (loop for c in slots collect
                         (to-mysql-value e c))))
        ;; Actually query
        (handler-case
            (apply #'dbi:execute (append (list (dbi:prepare db query)) params))
          (dbi:<dbi-database-error> (e)
            (if (= 1062 (slot-value e 'dbi.error::error-code))
                (error 'insert-duplicate-error)
                (error e))))
        ;; Get the last insert ID if it wasn't specified, otherwise just return
        ;; the specified id
        (if (and (slot-boundp e 'id) (slot-value e 'id))
            (slot-value e 'id)
            (nth 1 (dbi:fetch
                    (dbi:execute
                     (dbi:prepare db "SELECT LAST_INSERT_ID()")))))))))

(defun gen-connect-sql (e0 e1)
  (let* ((e0-name (type-of e0))
         (e1-name (type-of e1))
         (e0-sql-name (kebab-to-snake-case (string e0-name)))
         (e1-sql-name (kebab-to-snake-case (string e1-name))))
    (if (or (not (getf *m2m-meta* e0-name))
             (not (getf (getf *m2m-meta* e0-name) e1-name)))
        (error "Tried to call connect with 2 entities that are not joined in a m2m
      relationship. Maybe have a look at func def-many-to-many?"))
    (let* ((table-name (getf (getf *m2m-meta* e0-name) e1-name))
           (sql (format nil "INSERT INTO ~a (~a_id, ~a_id) VALUES(?, ?)"
                        table-name e0-sql-name e1-sql-name)))
      sql
      )))

(defun connect (e0 e1)
  "Connect 2 entities that are joined via a m2m relationship."
  (let ((db (get-conn))
        (sql (gen-connect-sql e0 e1)))
    (handler-case
        (dbi:execute (dbi:prepare db sql)
                     (slot-value e0 'id) (slot-value e1 'id))
      (dbi:<dbi-database-error> (e)
        (if (= 1062 (slot-value e 'dbi.error::error-code))
            (error 'insert-duplicate-error)
            (error e))))))

(defun gen-disconnect-sql (e0 e1)
  (let* ((e0-name (type-of e0))
         (e1-name (type-of e1))
         (e0-sql-name (kebab-to-snake-case (string e0-name)))
         (e1-sql-name (kebab-to-snake-case (string e1-name))))
    (if (or (not (getf *m2m-meta* e0-name))
            (not (getf (getf *m2m-meta* e0-name) e1-name)))
        (error "Tried to call connect with 2 entities that are not joined in a m2m
      relationship. Maybe have a look at func def-many-to-many?"))
    (let* ((table-name (getf (getf *m2m-meta* e0-name) e1-name))
           (sql (format nil "DELETE FROM ~a WHERE ~a_id = ? AND ~a_id = ?"
                        table-name e0-sql-name e1-sql-name))) sql
      )))

(defun disconnect (e0 e1)
  "Disconnect 2 m2m entities"
  (let ((db (get-conn))
        (sql (gen-disconnect-sql e0 e1)))
    (dbi:execute (dbi:prepare db sql)
                 (slot-value e0 'id) (slot-value e1 'id))))
