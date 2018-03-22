(defmacro init-args-from-row (slots row)
  "From a list of slots & a mysql row, generate a list of initargs for an entity"
  `(loop
      for (k v) on ,row by #'cddr
      for s in ',slots
      append (list (intern (string s) :keyword) v)))

(defmacro select-all (e)
  "Select all entities of the given type"

  ;; Get slots of e & the table namt
  (let ((slots (mapcar #'sb-mop:slot-definition-name
                       (sb-mop:class-direct-slots (class-of (make-instance e)))))
        (table-name (kebab-to-snake-case (string e))))
    ;; Get slots as column names
    (let ((columns (mapcar (lambda (s) (kebab-to-snake-case (string s))) slots)))
      ;; Build query
      (let ((query (format nil "SELECT ~{~a~^, ~} FROM ~a" columns table-name)))
        ;; Execute query & put into the entity
        `(loop for row in (dbi:fetch-all (dbi:execute (dbi:prepare *db* ,query)))
            ;; Make instance from initargs
            collect (apply #'make-instance
                           (append (list ',e) (init-args-from-row ,slots row))))))))
