;; Connect to DB
(defvar *db* (dbi:connect :mysql :database-name "test" :username "root" :password ""))

(defun kebab-to-snake-case (name)
  "Convert the string from a string separated with '-' to a string separated with '_'"
  (reduce (lambda (&optional s0 s1) (concatenate 'string (string s0) (string s1)))
          (loop for c across name collect (if (char= c #\-) #\_ (char-downcase c)))))

(defun snake-to-kebab-case (name)
  "Convert the string from a string separated with '_' to a string separated with '-'"
  (reduce (lambda (&optional s0 s1) (concatenate 'string (string s0) (string s1)))
          (loop for c across name collect (if (char= c #\_) #\- (char-downcase c)))))

(defun sql-mod-from-keyword (s)
  "Converts a SQL mod keyword to a valid string sql modifier. For example,
  :primary maps to \"PRIMARY KEY\". Passing a string in as s simply makes this a
  passthrough function, and returns the input."
  (cond
    ((typep s 'string) s)
    ((eq s :primary) "PRIMARY KEY")
    ((eq s :default) "DEFAULT")
    ((eq s :not-null) "NOT NULL")
    ((eq s :auto-increment) "AUTO_INCREMENT")))

(defun slot-to-column-def (s)
  "Given a list like (id \"BIGINT UNSIGNED\" :primary :auto-increment), turns it
  into a string like \"id BIGINT UNSIGNED PRIMARY KEY AUTO_INCREMENT\"."

  (concatenate 'string
               (kebab-to-snake-case (string (nth 0 s))) " " ; Name
               (string (nth 1 s)) " " ; Type
               ;; Reduce whatever remains to a string
               (reduce (lambda (&optional s0 s1) (concatenate 'string s0 " " s1))
                       (loop for m in (cdr (cdr s)) collect (sql-mod-from-keyword m)))))

(define-condition entity-already-exists (condition) ())

(defmacro defentity (name slots &optional override)
  "Define an entity with the given name. This macro creates a class with the
  entity of that name, and creates the appropriate corresponding persistence
  storage. You can create a new entity with the make-<name> function.

  Slots can be given to type the data for storage in a database. Each slot is a
  list, the first item being the name of the slot, the second being the type in
  database storage, and the following items being data regarding other storage
  modifiers.

  Example usage:

  ;; Define a user entity
  (defentity user
      ((id \"BIGINT UNSIGNED\" :primary :auto-increment)
      (first-name \"VARCHAR (256)\" :not-null)
      (last-name \"VARCHAR (256)\" :not-null)
      ))

  An optional 'override' argument can be set to T to drop the SQL table before
  re-creating it."


  ;; First, loop & extract out the column definitions from the slots, and also
  ;; the slot names.
  (let ((sql-def
         (concatenate
          'string
          "CREATE TABLE IF NOT EXISTS "
          (kebab-to-snake-case (string name))
          " ("
          (reduce (lambda (s0 s1) (format nil "~a,~%~a" s0 s1))
                  (loop for s in slots collect (slot-to-column-def s)))
          ");"))
        (slot-names (loop for s in slots collect
                         `(,(car s) :initform NIL :initarg
                            ,(intern (string (car s)) :keyword)))))
    `(progn
       (if ,override (dbi:execute (dbi:prepare *db* (concatenate
                                                     'string "DROP TABLE IF EXISTS "
                                                     ,(kebab-to-snake-case (string name))))))
       (handler-case (dbi:execute (dbi:prepare *db* ,sql-def))
         (error (e) (if (= 1050 (slot-value e 'dbi.error::error-code))
                        (error 'entity-already-exists) (error e))))
       (defclass ,name () ,slot-names))
    ))

(defun insert-one (e)
  "Insert an entity into the DB. NIL slots amount to NULL, or the default column
  value. NIL as an auto-increment field for example will generate a new
  auto-increment ID.

  Returns the value of LAST_INSERT_ID(). This will be the previous value of
  last_insert_id() if the ID is specified in the entity (i.e. the Autoincrement
  value wasn't used), or the value of the ID if autoincrement is used."

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
                           (dbi:prepare *db* "SELECT LAST_INSERT_ID()"))))
        ))))
