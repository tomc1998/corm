
(defmacro defentity (name slots)
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
      ((id \"BIGINT UNSIGNED\" 'primary 'auto-increment)
      (first-name \"VARCHAR (256)\" 'not-null)
      (last-name \"VARCHAR (256)\" 'not-null)
      ))"

  ;; First, loop & extract out the column definitions from the slots, and also
  ;; the slot names.
  (let ((column-defs (loop for s in slots collect s))
        (slot-names (loop for s in slots collect
                         `(,(car s) :initarg
                            ,(intern (string (car s)) :keyword)))))
    `(defclass
       ,name () ,slot-names)
    ))

