(asdf:defsystem corm
  :description "An example HTTP server in lisp"
  :version "0.0.1"
  :author "Tom <thomascheng1998@gmail.com>"
  :licence "Public Domain"
  :components ((:file "main/main"))
  )

;; Testing ASDF system
(asdf:defsystem tests
  :depends-on (:corm :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:test-file "test/main"))
  )

(ql:quickload :prove)
