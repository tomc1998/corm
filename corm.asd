(asdf:defsystem corm
  :description "An example HTTP server in lisp"
  :version "0.0.1"
  :author "Tom <thomascheng1998@gmail.com>"
  :licence "Public Domain"
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
  :components ((:test-file "test/main"))
  )

(ql:quickload :prove)
