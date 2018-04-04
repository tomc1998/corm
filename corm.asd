(defpackage :corm (:use "COMMON-LISP")
            (:export
             defentity
             entity-already-exists
             select-tree
             insert-one

             id
             ))
(in-package :corm)

(asdf:defsystem corm
  :depends-on (:cl-dbi)
  :components (
               (:file "main/db")
               (:file "main/create" :depends-on ("main/db"))
               (:file "main/select" :depends-on ("main/db"))
               (:file "main/insert" :depends-on ("main/db"))
               (:file "main/main" :depends-on ("main/db"
                                               "main/create"
                                               "main/select"
                                               "main/insert"))))

;; Testing ASDF system
(asdf:defsystem tests
  :depends-on (:corm :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/main")
               (:test-file "test/select"))
  )

(ql:quickload :prove)
