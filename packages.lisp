(defpackage :corm (:use "COMMON-LISP")
            (:export
             get-conn

             defentity
             entity-already-exists
             select-tree
             insert-one
             insert-duplicate-error
             update-entity
             update-all
             delete-entity
             delete-all
             check-owner-eq

             connect
             disconnect

             is-slot-bool

             id
             ))
