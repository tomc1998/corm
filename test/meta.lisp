(in-package :corm)

(prove:plan 2)
(defentity meta-test-entity ((some-val "VARCHAR(10)") (some-bool "TINYINT (1)")))
(prove:is (is-slot-bool (make-instance 'meta-test-entity) 'some-val) nil)
(prove:ok (is-slot-bool (make-instance 'meta-test-entity) 'some-bool))
(prove:finalize)
