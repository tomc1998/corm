(in-package :corm)

;; Metadata surrounding entities. 
(defparameter *entity-meta* () "This is a plist of entity names to entity-meta objects.")
(defparameter *m2m-meta* () "A plist of entity m2m relations. Should be kept up
to date with both sides - for example, if a (b) is in the list, b (a) will also be
in the list. An entity name will be bound to a plist of entity names to table names.

For example, after
(def-many-to-many my-entity-1 my-entity-2)
m2m-meta would look something like this
(my-entity-1 (my-entity-2 \"my_entity_1_my_entity_2\")
 my-entity-2 (my-entity-1 \"my_entity_1_my_entity_2\"))
")

;; Contains data about an entity field. 
(defclass field-meta () ( ;; a symbol indicating the lisp slot name of this field
                         (name :initarg :name :initform nil)
                         ;; A string indicating the mysql type
                         (type :initarg :type :initform nil)))

(defun is-slot-bool (e slot)
  "Given an entity and a slot name, check if that slot name is a bool"
  (let* ((meta (getf *entity-meta* (type-of e)))
         (field (find-if (lambda (f) (eq (slot-value f 'name) slot)) (slot-value meta 'fields)))
         (type (string-downcase (slot-value field 'type))))
    (and (search "tinyint" type) (search "(1)" type))))

(defclass entity-meta () ( ;; List of field-meta
                          (fields :initarg :fields :initform ())))

