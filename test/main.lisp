(defpackage :corm/main/main (:use :cl :prove))

(setf prove:*default-reporter* :list)

(defun name-case-transform-tests ()
  (prove:plan 4)
  (prove:is (kebab-to-snake-case "my-lisp-name") "my_lisp_name"
            "Kebab to snake case conversion should work")
  (prove:is (kebab-to-snake-case "my_lisp_name") "my_lisp_name"
            "Kebab to snake case conversion should work")
  (prove:is (snake-to-kebab-case "my_lisp_name") "my-lisp-name"
            "Snake to kebab case conversion should work")
  (prove:is (snake-to-kebab-case "my-lisp-name") "my-lisp-name"
            "Snake to kebab case conversion should work")
  (prove:finalize)
  )

(defun sql-mod-from-keyword-tests ()
  (prove:plan 4)
  (prove:is (sql-mod-from-keyword :default) "DEFAULT")
  (prove:is (sql-mod-from-keyword :not-null) "NOT NULL")
  (prove:is (sql-mod-from-keyword :auto-increment) "AUTO_INCREMENT")
  (prove:is (sql-mod-from-keyword "Custom Value") "Custom Value")
  (prove:finalize)
  )

(defun column-def-tests ()
  (prove:plan 1)
  (prove:is (slot-to-column-def '(email "VARCHAR(256)" :not-null))
            "email VARCHAR(256) NOT NULL")
  (prove:finalize)
  )

(defun defentity-tests ()
  (prove:plan 5)

  ;; Test whether defentity works as expected
  (defentity test-entity
      ((name "VARCHAR(256)" :not-null)) () T)
  (let ((instance (make-instance 'test-entity :name "Tom")))
    (prove:ok instance
              "Test entity structure should have a defined constructor")
    (prove:is (slot-value instance 'name) "Tom"
              "Slot 'name should be initialised properly through initargs")
    (setf (slot-value instance 'name) "Joe")
    (prove:is (slot-value instance 'name) "Joe"
              "Slot 'name should be changed when using setf"))

  ;; Test override def
  (defentity test-entity
      ((email "VARCHAR(1024)" :not-null)) () T)
  (prove:ok (make-instance 'test-entity :email "a@a.a"))
  (prove:is (slot-value (make-instance 'test-entity :email "a@a.a") 'email) "a@a.a")

  ;; Test defentity child
  (defentity test-entity-child
      ((some-data "VARCHAR(2048)" :not-null)) (test-entity) T)

  (prove:finalize))

(defun insert-one-tests ()
  (prove:plan 2)
  (prove:is (insert-one (make-instance 'test-entity :email "a@a.a")) 1
            "Inserting should return the proper auto-insert ID")
  (prove:is (insert-one (make-instance 'test-entity :id 234 :email "a@a.a")) 1
            "Inserting should return the old value of last insert ID if the ID
            of the entity was specified")
  (prove:finalize)
  )

(name-case-transform-tests)
(sql-mod-from-keyword-tests)
(column-def-tests)
(defentity-tests)
(insert-one-tests)
