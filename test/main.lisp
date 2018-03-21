(defpackage :corm/main/main (:use :cl :prove))

(prove:plan 3)

;; Test whether defentity works as expected
(defentity test-entity
    ((id "BIGINT UNSIGNED" :primary :auto-increment)
     (name "VARCHAR(256)" :not-null)))
(let ((instance (make-instance 'test-entity :id 123 :name "Tom")))
  (prove:ok instance
   "Test entity structure should have a defined constructor")
  (prove:is (slot-value instance 'id) 123
            "Slot 'id should be initialised properly through initargs")
  (prove:is (slot-value instance 'name) "Tom"
            "Slot 'name should be initialised properly through initargs")
  (setf (slot-value instance 'id) 234)
  (prove:is (slot-value instance 'id) 234
            "Slot 'id should be changed when using setf")
  )

(prove:finalize)
