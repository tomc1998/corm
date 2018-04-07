(in-package :corm)

(defun generate-where-clause-tests ()
  (prove:plan 2)
  (prove:is (generate-where-clause '(user post comment) '(and (not (comment id)) (= (user id) (post id))))
            '("((NOT 2_comment.id) AND (0_user.id = 1_post.id))")
            "generate-where-clause works")
  (prove:is (generate-where-clause '(user) '(or (= (user id) 1) (= (user id) 2)))
            '("((0_user.id = ?) OR (0_user.id = ?))" 1 2)
            "generate-where-clause correctly generates prepared statement args")
  (print (generate-where-clause '(user-auth) '(= (user-auth user) "asd")))
  (prove:finalize)
  )

(defun build-visit-list-tests ()
  (prove:plan 1)
  (prove:is (build-visit-list-from-select-tree '(user ((post ((comment ()))))))
            '(user post comment))
  (prove:finalize))

(defun build-sql-column-spec-tests ()
  (prove:plan 1)
  (defentity select-test-entity ((email "VARCHAR(256)" :not-null)) ())
  (prove:is (build-sql-column-spec-from-entity 'select-test-entity "0")
            "0_select_test_entity.id AS 0_id, 0_select_test_entity.email AS 0_email")
  (prove:finalize))

(defun build-join-list-test ()
  (prove:plan 1)
  (defentity user ((email "VARCHAR(256)" 'not-null)) () T)
  (defentity post ((body "VARCHAR(256)" 'not-null)) (user) T)
  (defentity comment ((body "VARCHAR(256)" 'not-null)) (post) T)
  (prove:is (build-join-list-from-visit-list '(user ((post ((comment ()))))))
            "
LEFT JOIN post AS 1_post ON 1_post.parent_user_id = 0_user.id
LEFT JOIN comment AS 2_comment ON 2_comment.parent_post_id = 1_post.id"
            "Join list should be build correctly"))

(defun select-tree-tests ()
  (prove:plan 2)
  (insert-one (make-instance 'user :email "a@a.a"))
  (insert-one (make-instance 'post :parent-user-id 1 :body "aaa000"))
  (insert-one (make-instance 'post :parent-user-id 1 :body "aaa111"))
  (insert-one (make-instance 'post :parent-user-id 1 :body "aaa222"))
  (insert-one (make-instance 'comment :parent-post-id 1 :body "commentaaa000"))
  (insert-one (make-instance 'comment :parent-post-id 1 :body "commentaaa111"))
  (insert-one (make-instance 'comment :parent-post-id 2 :body "commentaaa222"))
  (insert-one (make-instance 'user :email "a@a.a"))

  ;; TODO: How to test the correct tree response? Pretty awkward.
  (let ((tree (select-tree '(user ((post ((comment ()))))))))
    (prove:is (length tree) 2))
  (let ((tree (select-tree '(user ((post ((comment ()))))) :where '(= (user id) 1))))
    (prove:is (length tree) 1))
  (prove:finalize))

(generate-where-clause-tests)
(build-visit-list-tests)
(build-sql-column-spec-tests)
(build-join-list-test)

(select-tree-tests)
