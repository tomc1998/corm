(defpackage :corm (:use "COMMON-LISP")
            (:export
             defentity
             entity-already-exists
             select-tree
             insert-one
             insert-duplicate-error
             update-entity
             delete-entity
             delete-all
             check-owner-eq

             is-slot-bool

             id
             ))
(in-package :corm)

(asdf:defsystem corm
  :depends-on (:cl-dbi)
  :components (
               (:file "main/db")
               (:file "main/meta" :depends-on ("main/db"))
               (:file "main/create" :depends-on ("main/db" "main/meta"))
               (:file "main/select" :depends-on ("main/db"))
               (:file "main/insert" :depends-on ("main/db"))
               (:file "main/update" :depends-on ("main/db"))
               (:file "main/delete" :depends-on ("main/db"))
               (:file "main/main" :depends-on ("main/db"
                                               "main/create"
                                               "main/select"
                                               "main/insert"))))

;; Testing ASDF system
(asdf:defsystem corm-tests
  :depends-on (:corm :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/main")
               (:test-file "test/create")
               (:test-file "test/update")
               (:test-file "test/meta")
               (:test-file "test/delete")
               (:test-file "test/select")))

(ql:quickload :prove)
