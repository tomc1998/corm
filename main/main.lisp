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
                         `(,(car s) :initarg
                            ,(intern (string (car s)) :keyword)))))
    (print sql-def)
    `(progn
       (if ,override (dbi:execute (dbi:prepare *db* (concatenate
                                                     'string "DROP TABLE IF EXISTS "
                                                     ,(kebab-to-snake-case (string name))))))
       (handler-case (dbi:execute (dbi:prepare *db* ,sql-def))
         (error (e) (if (= 1050 (slot-value e 'dbi.error::error-code))
                        (error 'entity-already-exists) (error e))))
       (defclass ,name () ,slot-names))
    ))

