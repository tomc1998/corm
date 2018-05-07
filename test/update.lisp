(in-package :corm)

(defentity update-test-entity ((val-a "INTEGER") (val-b "INTEGER")) :override T)

(prove:plan 1)
(prove:is
 (generate-update-expressions 'val-a 'val-b)
 "val_a = ?, val_b = ?"
 "Should generate update expressions correctly (i.e. X = ..., Y = ... etc.)"
 )
(prove:finalize)

(prove:plan 1)
(prove:is
 (generate-update-sql
  (make-instance 'update-test-entity :val-a 100 :val-b 200 :id 1)
  'val-a 'val-b)
 "UPDATE update_test_entity SET val_a = ?, val_b = ? WHERE id = ?"
 "Should generate update statement correctly")
(prove:finalize)

(prove:plan 1)
(let ((e (make-instance 'update-test-entity :val-a 100)))
  (setf (slot-value e 'id) (insert-one e))
  (setf (slot-value e 'val-a) 200)
  (update-entity e 'val-a)
  (let ((selected
         (select-tree '(update-test-entity ())
                      :where `(= (update-test-entity id)
                               ,(slot-value e 'id)))))
    (prove:is (slot-value (caar selected) 'val-a) 200)))
(prove:finalize)

(prove:plan 2)
(let ((e0 (make-instance 'update-test-entity :val-a 100))
      (e1 (make-instance 'update-test-entity :val-a 100)))
  (setf (slot-value e0 'id) (insert-one e0))
  (setf (slot-value e1 'id) (insert-one e1))
  (setf (slot-value e0 'val-a) 200)
  (setf (slot-value e1 'val-a) 300)
  (update-all (list e0 e1) 'val-a)
  (let ((selected
         (select-tree
          '(update-test-entity ())
          :where `(or
                   (= (update-test-entity id) ,(slot-value e0 'id))
                   (= (update-test-entity id) ,(slot-value e1 'id))))))
    (prove:is
     (slot-value
      (car (find-if (lambda (e) (= (slot-value e0 'id)
                               (slot-value (car e) 'id)))
                selected))
      'val-a)
     200)
    (prove:is
     (slot-value
      (car (find-if (lambda (e) (= (slot-value e1 'id)
                               (slot-value (car e) 'id)))
                selected))
      'val-a)
     300)
    ))

(prove:finalize)
