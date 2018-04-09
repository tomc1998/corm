(in-package :corm)

(defentity update-test-entity ((val-a "INTEGER") (val-b "INTEGER")) () T)

(prove:plan 1)
(prove:is
 (generate-update-expressions
  (make-instance 'update-test-entity :val-a 100 :val-b 200)
  'val-a 'val-b)
 "val_a = 100, val_b = 200"
 "Should generate update expressions correctly (i.e. X = ..., Y = ... etc.)"
 )
(prove:finalize)

(prove:plan 1)
(prove:is
 (generate-update-sql
  (make-instance 'update-test-entity :val-a 100 :val-b 200 :id 1)
  'val-a 'val-b)
 "UPDATE update_test_entity SET val_a = 100, val_b = 200 WHERE id = ?"
 "Should generate update statement correctly")
(prove:finalize)

(prove:plan 1)
(let ((e (make-instance 'update-test-entity :val-a 100)))
  (setf (slot-value e 'id) (insert-one e))
  (setf (slot-value e 'val-a) 200)
  (update e 'val-a)
  (let ((selected
         (select-tree '(update-test-entity ())
                      :where `(= (update-test-entity id)
                               ,(slot-value e 'id)))))
    (prove:is (slot-value (caar selected) 'val-a) 200)))
(prove:finalize)
