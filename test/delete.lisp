(in-package :corm)

(defentity delete-entity-test ((some-val "INTEGER")) :override T)

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

(prove:plan 1)
(prove:is (generate-delete-all-sql (list (make-instance 'delete-entity-test :id 1)
                                         (make-instance 'delete-entity-test :id 2)
                                         (make-instance 'delete-entity-test :id 3)
                                         (make-instance 'delete-entity-test :id 4)))
          "DELETE FROM delete_entity_test WHERE id = ? OR id = ? OR id = ? OR id = ?")
(prove:is (generate-delete-all-sql (list (make-instance 'delete-entity-test :id 1)))
          "DELETE FROM delete_entity_test WHERE id = ?")
(prove:finalize)

(let ((e1 (make-instance 'delete-entity-test))
      (e2 (make-instance 'delete-entity-test))
      (e3 (make-instance 'delete-entity-test)))
  (setf (slot-value e1 'id) (insert-one e1))
  (setf (slot-value e2 'id) (insert-one e2))
  (setf (slot-value e3 'id) (insert-one e3))
  (prove:plan 4)
  (prove:is (length (select-tree '(delete-entity-test ()))) 3)
  (delete-all (list e1 e2))
  (let ((tree (select-tree '(delete-entity-test ()))))
    (prove:is (length tree) 1)
    (prove:is (slot-value (caar tree) 'id) (slot-value e3 'id)))
  (delete-all (list e3))
  (prove:is (length (select-tree '(delete-entity-test ()))) 0)
  (prove:finalize)
  )
