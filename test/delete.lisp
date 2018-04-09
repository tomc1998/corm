(in-package :corm)

(defentity delete-entity-test ((some-val "INTEGER")) () T)

(let ((e (make-instance 'delete-entity-test)))
  (prove:plan 2)
  (setf (slot-value e 'id) (insert-one e))
  (prove:is (length (select-tree '(delete-entity-test ()))) 1)
  (delete-entity e)
  (prove:is (length (select-tree '(delete-entity-test ()))) 0)
  (prove:finalize)
  )

(prove:plan 1)
(prove:is (generate-delete-sql (make-instance 'delete-entity-test))
          "DELETE FROM delete_entity_test WHERE id = ?")
(prove:finalize)
