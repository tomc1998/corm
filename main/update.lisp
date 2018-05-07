(in-package :corm)

(defun generate-update-expressions (&rest fields)
  "Generates the X = ?, X = ?, ... string given the entity e and the fields to
update 'fields'."
  (format nil "~{~a = ?~^, ~}"
          (loop for f in fields append
               (list (kebab-to-snake-case (string f))))))

(defun generate-batch-update-expressions (e-list &rest fields)
  "Generates the X = ?, X = ?, ... string given the entity e and the fields to
update 'fields'."
  (format nil "~{~a = (CASE id ~{WHEN ~a THEN ? ~} END)~^, ~}"
          (loop for f in fields append
               (list (kebab-to-snake-case (string f))
                     (loop for e in e-list collect
                          (slot-value e 'id))))))

(defun generate-update-sql (e &rest fields)
  "Generate some SQL to update the given entity's fields"
  (format nil "UPDATE ~a SET ~a WHERE id = ?"
          (kebab-to-snake-case (string (type-of e)))
          (apply #'generate-update-expressions fields)))

(defun generate-batch-update-sql (e-list &rest fields)
  "Generate some SQL to update the given entity's fields"
  (format nil "UPDATE ~a SET ~a WHERE TRUE AND (~{id = ?~*~^ OR ~})"
          (kebab-to-snake-case (string (type-of (car e-list))))
          (print (apply #'generate-batch-update-expressions e-list fields))
          e-list
          ))

(defun update-entity (e &rest fields)
  "Update the specified fields on a given entity. The entity's ID will be used
  to select the entity, and changes in the fields will be applied according to
  the given entity.
  # Example
  (defentity my-entity ((some-field \"INTEGER\")))
  (let ((e (make-instance 'my-entity :some-field 110)))
    ;; Insert the entity, capturing the autoincremented ID
    (setf (slot-value e 'corm:id) (insert-one e))
    ;; Perform update
    (setf (slot-value e 'some-field') 20)
    ;; Update the entity's 'some-field' value in the database
    (update e 'some-field)
    ;; some-field is now 20 in the DB
    )
   This example would have generated SQL on the lines of:
   UPDATE my_entity SET some_field = 20 WHERE my_entity.id = 1;
   Given that the entity's auto-increment ID was set to 1 on creation "
  (let ((sql (apply #'generate-update-sql (append (list e) fields))))
    (apply #'dbi:execute (append (list (dbi:prepare (get-conn) sql))
                                 (loop for f in fields collect (to-mysql-value e f))
                                 (list (slot-value e 'id) )))
    ))

(defun update-all (e-list &rest fields)
  "See update-entity. Identical, but accepts a list of entities, rather than
just one."
  (let ((sql (apply #'generate-batch-update-sql (append (list e-list) fields))))
    (apply #'dbi:execute
           (append (list (dbi:prepare (get-conn) sql))
                   (loop for f in fields append
                        (loop for e in e-list collect
                             (to-mysql-value e f)))
                   (mapcar (lambda (e) (slot-value e 'id)) e-list)))
    )
  )
