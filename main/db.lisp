(in-package :corm)

;; Connect to DB
(defvar *db* (dbi:connect :mysql :database-name "test" :username "root" :password ""))


;; Some utils for names

(defun to-mysql-value (e slot)
  "Given an entity and a slot name, convert it to a value ready to be put into
params"
  (let ((v (slot-value e slot)))
    (if (is-slot-bool e slot) (if v 1 0)
        v
        )))

(defun kebab-to-snake-case (name)
  "Convert the string from a string separated with '-' to a string separated with '_'"
  (reduce (lambda (&optional s0 s1) (concatenate 'string (string s0) (string s1)))
          (loop for c across name collect (if (char= c #\-) #\_ (char-downcase c)))))

(defun snake-to-kebab-case (name)
  "Convert the string from a string separated with '_' to a string separated with '-'"
  (reduce (lambda (&optional s0 s1) (concatenate 'string (string s0) (string s1)))
          (loop for c across name collect (if (char= c #\_) #\- (char-downcase c)))))

