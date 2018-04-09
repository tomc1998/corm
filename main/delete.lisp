(in-package :corm)

(defun generate-delete-sql (e)
  (format nil "DELETE FROM ~a WHERE id = ?" (kebab-to-snake-case (string (type-of e)))))

(defun delete-entity (e)
  "Delete the entity with the given ID from the db."
  (let ((sql (generate-delete-sql e)))
    (dbi:execute (dbi:prepare *db* sql) (slot-value e 'id))))
