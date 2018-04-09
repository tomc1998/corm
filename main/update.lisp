(in-package :corm)

(defun generate-update-expressions (&rest fields)
  "Generates the X = ?, X = ?, ... string given the entity e and the fields to
update 'fields'."
  (format nil "~{~a = ?~^, ~}"
          (loop for f in fields append
               (list (kebab-to-snake-case (string f))))))

(defun generate-update-sql (e &rest fields)
  "Generate some SQL to update the given entity's fields"
  (format nil "UPDATE ~a SET ~a WHERE id = ?"
          (kebab-to-snake-case (string (type-of e)))
          (apply #'generate-update-expressions fields)))

(defun update (e &rest fields)
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
    (apply #'dbi:execute (append (list (dbi:prepare *db* sql))
                                 (loop for f in fields collect
                                      (let ((v (slot-value e f)))
                                        (if (is-slot-bool e f) (if v 1 0) v)))
                                 (list (slot-value e 'id) )))
    ))
