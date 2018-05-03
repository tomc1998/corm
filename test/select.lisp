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
  (defentity select-test-entity ((email "VARCHAR(256)" :not-null)))
  (prove:is (build-sql-column-spec-from-entity 'select-test-entity "0")
            "0_select_test_entity.id AS 0_id, 0_select_test_entity.email AS 0_email")
  (prove:finalize))

(defun build-join-from-entities-test ()
  (prove:plan 2)
  (def-many-to-many my-e-0 my-e-1)
  (prove:is (build-join-from-entities 0 1 'my-e-0 'my-e-1)
            (concatenate
             'string
             "INNER JOIN my_e_0_my_e_1 ON my_e_0_my_e_1.my_e_0_id = 0_my_e_0.id"
             " "
             "INNER JOIN my_e_1 AS 1_my_e_1 ON my_e_0_my_e_1.my_e_1_id = 1_my_e_1.id"
             ))
  (prove:is (build-join-from-entities 0 1 'e0 'e1)
            "LEFT JOIN e1 AS 1_e1 ON 1_e1.parent_e0_id = 0_e0.id")
  (prove:finalize))

(defun build-join-list-test ()
  (prove:plan 1)
  (defentity user ((email "VARCHAR(256)" 'not-null)) :override T)
  (defentity post ((body "VARCHAR(256)" 'not-null)) :parents (user) :override T)
  (defentity comment ((body "VARCHAR(256)" 'not-null)) :parents (post) :override T)
  (prove:is (build-join-list-from-visit-list '(user ((post ((comment ()))))))
            "
LEFT JOIN post AS 1_post ON 1_post.parent_user_id = 0_user.id
LEFT JOIN comment AS 2_comment ON 2_comment.parent_post_id = 1_post.id"
            "Join list should be build correctly"))

(defun select-tree-tests ()
  (insert-one (make-instance 'user :email "a@a.a"))
  (insert-one (make-instance 'post :parent-user-id 1 :body "aaa000"))
  (insert-one (make-instance 'post :parent-user-id 1 :body "aaa111"))
  (insert-one (make-instance 'post :parent-user-id 1 :body "aaa222"))
  (insert-one (make-instance 'comment :parent-post-id 1 :body "commentaaa000"))
  (insert-one (make-instance 'comment :parent-post-id 1 :body "commentaaa111"))
  (insert-one (make-instance 'comment :parent-post-id 2 :body "commentaaa222"))
  (insert-one (make-instance 'user :email "a@a.a"))

  ;; TODO: How to test the correct tree response? Pretty awkward.
  (prove:plan 3)
  (let ((tree (select-tree '(user ((post ((comment ()))))))))
    (prove:is (length tree) 2))
  (let ((tree (select-tree '(user ((post ((comment ()))))) :where '(= (user id) 1))))
    (prove:is (length tree) 1))
  (let ((tree (select-tree '(user ((post ((comment ()))))) :where '(like (user email) "a@a%"))))
    (prove:is (length tree) 2))
  (prove:finalize))

(defun check-owner-eq-tests ()
  (defentity user ((email "VARCHAR(256)" 'not-null)) :override T)
  (defentity post ((body "VARCHAR(256)" 'not-null)) :parents (user) :override T)
  (let* (
        (u0 (make-instance 'user :id 100))
        (u1 (make-instance 'user :id 101))
        (p0 (make-instance 'post :id 1 :parent-user-id 100))
        (p1 (make-instance 'post :id 2 :parent-user-id 101)))
    (insert-one u0)
    (insert-one u1)
    (insert-one p0)
    (insert-one p1)
    (prove:ok (check-owner-eq p0 'parent-user-id (slot-value u0 'id)) "check-owner-eq should return t")
    (prove:ok (check-owner-eq p1 'parent-user-id (slot-value u1 'id)) "check-owner-eq should return t")
    (prove:is (check-owner-eq p1 'parent-user-id (slot-value u0 'id)) nil "check-owner-eq should return nil")
    (prove:is (check-owner-eq p0 'parent-user-id (slot-value u1 'id)) nil "check-owner-eq should return nil")
    ))

(generate-where-clause-tests)
(build-visit-list-tests)
(build-sql-column-spec-tests)
(build-join-from-entities-test)
(build-join-list-test)

(select-tree-tests)
(check-owner-eq-tests)
