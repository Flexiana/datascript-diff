(ns diff-test
  (:require
    [clojure.test :refer :all]
    [diff :refer [make-it
                  to-delete
                  to-add]]))


(defn test-it
  [a b]
  (is (= b (make-it a b))))


(deftest diff-test)


(defn to-add-rm-tmp
  [param1 param2]
  (dissoc (to-add param1 param2) :tmp))


(is (= {} (to-add-rm-tmp {} {})))
(is (= {} (to-add-rm-tmp {:a 1} {:a 1})))
(is (= {:+ {[:a] 2}} (to-add-rm-tmp {} {:a 2})))
(is (= {:+ {[:b] 2 [:c] 3}} (to-add-rm-tmp {:a 1} {:b 2 :c 3})))


(is (= {:- {[:a] 1} :+ {[:a] 2 [:b] 2 [:c] 3}}
       (to-add-rm-tmp {:a 1} {:a 2 :b 2 :c 3})))


(is (= {:+ {[:a] {:a 1}}} (to-add-rm-tmp {} {:a {:a 1}})))

(is (= {:+ {[:a] {:a 1}}} (to-add-rm-tmp {:b 2} {:a {:a 1}})))


(is (= {:- {[:a] 2}
        :+ {[:a] {:a 1}
            [:b] 3}}
       (to-add-rm-tmp {:a 2} {:a {:a 1}
                              :b 3})))


(is (= {:+ {[:b] 3}}
       (to-add-rm-tmp {:a {:a 1}} {:a {:a 1}
                                   :b 3})))


(is (= {:+ {[:a :a] 2
            [:b]    3}
        :- {[:a :a] 1}}
       (to-add-rm-tmp {:a {:a 1}} {:a {:a 2}
                                   :b 3})))


(is (= {:- {[:b] [1 2 3 4 5 6]}
        :+ {[:b] [4 5 6]}}
       (to-add-rm-tmp {:b [1 2 3 4 5 6]} {:b [4 5 6]})))


(to-delete {:a "a"} {})
(to-delete {:a "c"} {:a "a"})
(to-delete {:a "c" :b 2} {:a "a"})

(test-it {} {})
(test-it {:a 1} {:a 2})
(test-it {:a 1} {:b 2})
(test-it {:a 1} {:a 2})
(test-it {:a 1} {:a 2})
(test-it {:a 1} {:a 2})
