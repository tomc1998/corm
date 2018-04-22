(in-package :corm)

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
  (prove:finalize))

(defun to-mysql-value-tests ()
  (defentity value-test-entity ((some-val "VARCHAR(256)") (some-bool "TINYINT(1)")) :override t)
  (prove:plan 2)
  (let ((e (make-instance 'value-test-entity :some-val "hello" :some-bool t)))
    (prove:is (to-mysql-value e 'some-val) "hello")
    (prove:is (to-mysql-value e 'some-bool) 1))
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
      ((name "VARCHAR(256)" :not-null :unique)) :override T)
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
      ((email "VARCHAR(1024)" :not-null)) :override T)
  (prove:ok (make-instance 'test-entity :email "a@a.a"))
  (prove:is (slot-value (make-instance 'test-entity :email "a@a.a") 'email) "a@a.a")

  ;; Test defentity child
  (defentity test-entity-child
      ((some-data "VARCHAR(2048)" :not-null)) :parents (test-entity) :override T)

  (prove:finalize))

(defun insert-one-tests ()
  (defentity test-entity
      ((email "VARCHAR(1024)" :not-null)) :override T)
  (prove:plan 2)
  (prove:is (insert-one (make-instance 'test-entity :email "a@a.a")) 1
            "Inserting should return the proper auto-insert ID")
  (prove:is (insert-one (make-instance 'test-entity :id 234 :email "b@b.b")) 234
            "Inserting should return the ID if the ID of the entity was
            specified")
  (prove:finalize))

(defun duplicate-key-tests ()
  (defentity dup-test-entity
      ((email "VARCHAR(1024)" :not-null :unique)) :override T)
  (prove:plan 3)
  (prove:is (insert-one (make-instance 'dup-test-entity :email "asd")) 1)
  (prove:is (handler-case (insert-one (make-instance 'dup-test-entity :email "asd"))
              (insert-duplicate-error () :unique-error))
            :unique-error "Unique error should be thrown as such")
  (defclass non-entity () ((email :initarg :email)))
  (prove:is (handler-case (insert-one (make-instance 'non-entity :email "asd"))
              (insert-duplicate-error () :unique-error)
              (condition () :error)) :error "Non-unique error shouldn't flag as unique")
  (prove:finalize))

(name-case-transform-tests)
(to-mysql-value-tests)
(sql-mod-from-keyword-tests)
(column-def-tests)
(defentity-tests)
(insert-one-tests)
(duplicate-key-tests)
