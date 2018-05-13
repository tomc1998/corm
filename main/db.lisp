(in-package :corm)

;; Connect to DB
(defvar *pool*)
(defvar *curr-conn-ix*)

(defun connect-to-db ()
  (dbi:connect :mysql :database-name "test"
               :username "root" :password ""))

(defun create-pool (size)
  (setf *pool* (loop for i from 0 to size collect (connect-to-db)))
  (setf *curr-conn-ix* 0))

(create-pool 10)

(defun advance-conn-ix ()
  "Advance *curr-conn-ix* circularly (when we hit the end, loop back to 0)"
  (setf *curr-conn-ix* (1+ *curr-conn-ix*))
  (if (>= *curr-conn-ix* (length *pool*))
      (setf *curr-conn-ix* 0))
  )

(defun get-conn ()
  (let ((conn (nth *curr-conn-ix* *pool*)))
    (loop while (not (dbi:ping conn)) do
         (setf (nth *curr-conn-ix* *pool*) (connect-to-db))
         (advance-conn-ix)
         (setf conn (nth *curr-conn-ix* *pool*)))
    (advance-conn-ix)
    conn))

;; Some utils for names
(defun to-mysql-value (e slot)
  "Given an entity and a slot name, convert it to a value ready to be put into
params"
  (let ((v (slot-value e slot)))
    (cond
      ((is-slot-bool e slot) (if v 1 0))
      ((numberp v) (format nil "~$" v))
      (t v))
    ))

(defun kebab-to-snake-case (name)
  "Convert the string from a string separated with '-' to a string separated with '_'"
  (reduce (lambda (&optional s0 s1) (concatenate 'string (string s0) (string s1)))
          (loop for c across name collect (if (char= c #\-) #\_ (char-downcase c)))))

(defun snake-to-kebab-case (name)
  "Convert the string from a string separated with '_' to a string separated with '-'"
  (reduce (lambda (&optional s0 s1) (concatenate 'string (string s0) (string s1)))
          (loop for c across name collect (if (char= c #\_) #\- (char-downcase c)))))

