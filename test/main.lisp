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

;; Test sql to lisp names
(prove:is (kebab-to-snake-case "my-lisp-name") "my_lisp_name"
          "Kebab to snake case conversion should work")
(prove:is (kebab-to-snake-case "my_lisp_name") "my_lisp_name"
          "Kebab to snake case conversion should work")
(prove:is (snake-to-kebab-case "my_lisp_name") "my-lisp-name"
          "Snake to kebab case conversion should work")
(prove:is (snake-to-kebab-case "my-lisp-name") "my-lisp-name"
          "Snake to kebab case conversion should work")

;; Test keywords to SQL mods
(prove:is (sql-mod-from-keyword :primary) "PRIMARY KEY")
(prove:is (sql-mod-from-keyword :default) "DEFAULT")
(prove:is (sql-mod-from-keyword :not-null) "NOT NULL")
(prove:is (sql-mod-from-keyword :auto-increment) "AUTO_INCREMENT")
(prove:is (sql-mod-from-keyword "Custom Value") "Custom Value")

;; Test creating column defs
(prove:is (slot-to-column-def '(id "BIGINT UNSIGNED" :primary :auto-increment))
          "id BIGINT UNSIGNED PRIMARY KEY AUTO_INCREMENT")

(prove:finalize)
