(in-package :corm)

;; Metadata surrounding entities. This is a plist of entity names to entity-meta objects.
(defparameter *entity-meta* ())

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

