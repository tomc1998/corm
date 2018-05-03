(in-package :corm)

(prove:plan 2)
(defentity test-insert-connect-e0 ())
(defentity test-insert-connect-e1 ())
(defentity test-insert-connect-e2 ())
(def-many-to-many test-insert-connect-e0 test-insert-connect-e1)
(let ((e0 (make-instance 'test-insert-connect-e0))
      (e1 (make-instance 'test-insert-connect-e1)))
  (setf (slot-value e0 'id) (insert-one e0))
  (setf (slot-value e1 'id) (insert-one e1))
  (prove:is (gen-connect-sql e0 e1)
            (concatenate
             'string
             "INSERT INTO test_insert_connect_e0_test_insert_connect_e1 "
             "(test_insert_connect_e0_id, test_insert_connect_e1_id) "
             "VALUES(?, ?)")))
(let ((e0 (make-instance 'test-insert-connect-e0))
      (e1 (make-instance 'test-insert-connect-e2)))
  (setf (slot-value e0 'id) (insert-one e0))
  (setf (slot-value e1 'id) (insert-one e1))
  (prove:is-error (gen-connect-sql e0 e1) 'error))
(prove:finalize)
