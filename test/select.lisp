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
  (prove:is (build-visit-list-from-select-tree '(user ((post ((comment ()))))))
            '(user post comment))
  (prove:finalize))

(defun build-sql-column-spec-tests ()
  (prove:plan 1)
  (defentity select-test-entity ((email "VARCHAR(256)" 'not-null)) ())
  (prove:is (build-sql-column-spec-from-entity 'select-test-entity "0")
            "select_test_entity.id AS 0_id, select_test_entity.email AS 0_email")
  (prove:finalize))

(defun build-join-list-test ()
  (prove:plan 1)
  (defentity user ((email "VARCHAR(256)" 'not-null)) ())
  (defentity post ((email "VARCHAR(256)" 'not-null)) (user))
  (defentity comment ((email "VARCHAR(256)" 'not-null)) (post))
  (prove:is (build-join-list-from-visit-list '(user ((post ((comment ()))))))
            "
 LEFT JOIN post AS 1_post ON 1_post.parent_user_id = 0_user.id
 LEFT JOIN comment AS 2_comment ON 2_comment.parent_post_id = 1_post.id")
  )

(select-all-tests)
(build-visit-list-tests)
(build-sql-column-spec-tests)
(build-join-list-test)
