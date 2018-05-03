(in-package :corm)

(prove:plan 1)
(prove:is (gen-many-to-many-def 'my-entity-1 'my-entity-2)
          "CREATE TABLE IF NOT EXISTS my_entity_1_my_entity_2 (
my_entity_1_id BIGINT UNSIGNED NOT NULL,
my_entity_2_id BIGINT UNSIGNED NOT NULL,
PRIMARY KEY (my_entity_1_id, my_entity_2_id));")
(prove:finalize)

(prove:plan 1)
(def-many-to-many my-entity-1 my-entity-2)
(prove:is (getf *m2m-meta* 'my-entity-1) '(my-entity-2 "my_entity_1_my_entity_2"))
(prove:is (getf *m2m-meta* 'my-entity-2) '(my-entity-1 "my_entity_1_my_entity_2"))
(prove:finalize)
