(in-package :corm)

(defun generate-delete-sql (e)
  (format nil "DELETE FROM ~a WHERE id = ?" (kebab-to-snake-case (string (type-of e)))))

(defun delete-entity (e)
  "Delete the entity with the given ID from the db."
  (let ((sql (generate-delete-sql e)))
    (dbi:execute (dbi:prepare (get-conn) sql) (slot-value e 'id))))

(defun generate-delete-all-sql (e-list)
  (format nil "DELETE FROM ~a WHERE ~{~*id = ?~^ OR ~}"
          (kebab-to-snake-case (string (type-of (car e-list)))) e-list))

(defun delete-all (e-list)
  "Given a list of entities (all of which should have the ID slot populated with
some value), delete these entities from the DB"
  (let ((sql (generate-delete-all-sql e-list))
        (params (remove-if #'null (mapcar (lambda (e) (slot-value e 'id)) e-list))))
    (apply #'dbi:execute (append (list (dbi:prepare (get-conn) sql)) params))
    ))
