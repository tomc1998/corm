(defun select-all-tests ()
  (prove:plan 4)
  (prove:is
   (init-args-from-row (id email) (list :row-id 123 :row-email "asd"))
   (list :id 123 :email "asd"))
  (let ((entity (nth 0 (select-all test-entity))))
    (prove:is (slot-value entity 'id) 1)
    (prove:is (slot-value entity 'email) "a@a.a"))
  (prove:is (length (select-all test-entity)) 2)
  (prove:finalize))

(defun build-visit-list-tests ()
  (prove:plan 1)
  (prove:is (build-visit-list-from-select-tree '(user (post (comment()))))
            '(user post comment))
  (prove:finalize))

(select-all-tests)
(build-visit-list-tests)
