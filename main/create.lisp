(defun sql-mod-from-keyword (s)
  "Converts a SQL mod keyword to a valid string sql modifier. For example,
  :primary maps to \"PRIMARY KEY\". Passing a string in as s simply makes this a
  passthrough function, and returns the input."
  (cond
    ((typep s 'string) s)
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

(defmacro defentity (name slots parents &optional override)
  "Define an entity with the given name. This macro creates a class with the
  entity of that name, and creates the appropriate corresponding persistence
  storage. You can create a new entity with the make-<name> function.

  Slots can be given to type the data for storage in a database. Each slot is a
  list, the first item being the name of the slot, the second being the type in
  database storage, and the following items being data regarding other storage
  modifiers.

  A list of parent entities & names for these can be given to create a
  many-to-one relationship between this entity and the given entities. For
  example, one may have a 'user' entity to represent users on a website. Users
  might be able to create posts, which are amalgamated into a central newsfeed.
  There would therefore be a many-to-one relationship between posts and users -
  users would 'own' many posts. Therefore, when defining the post entity, one
  would specify the user entity as a parent.
  This list is given as a plist, with the keyword being the alias for the
  relationship. In the above example, one may want to alias the post - user
  relationship as 'author', as the post is authored by a given user. You would
  then pass (:author user) as the parent list. This will then generate a
  corresponding SQL table column called 'parent_author_id', and can also be
  referenced in complex select queries.

  An autoincrement ID is automatically added to table definitions.

  Example usage:

  ;; Define a user entity
  (defentity user
      ;; Slots
      ((first-name \"VARCHAR (256)\" :not-null)
      (last-name \"VARCHAR (256)\" :not-null))
      ;; Parents (none)
      ())
  ;; Define a child 'post' entity
  (defentity post
      ;; Slots
      (body \"VARCHAR (2048)\" :not-null)
      ;; Parents
      (:author user))

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
          (format nil "~{parent_~a_id BIGINT UNSIGNED~^,~%~*~}"
                  (mapcar (lambda (s) (kebab-to-snake-case (string s))) parents))
          (reduce (lambda (s0 s1) (format nil "~a,~%~a" s0 s1))
                  (cons "id BIGINT UNSIGNED PRIMARY KEY AUTO_INCREMENT"
                        (loop for s in slots collect (slot-to-column-def s))))
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
       (defclass ,name () ,(append
                            (loop for (k v) on parents by #'cddr collect
                                 (list (intern (format nil "PARENT-~a-ID" (string-upcase k)))
                                       :initarg k :initform NIL))
                            (list '(id :initarg :id :initform NIL))
                            slot-names)))
    ))
