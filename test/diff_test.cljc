(ns diff-test
  (:require
    [clojure.test :refer :all]
    [diff :refer [make
                  ->remove
                  ->extend]]))


(deftest step-one
  (is (= {} (->extend {} {})))
  (is (= {} (->extend {:a 1} {:a 1})))
  (is (= {:+ {[:a] 2}} (->extend {} {:a 2})))
  (is (= {:+ {[:b] 2 [:c] 3}} (->extend {:a 1} {:b 2 :c 3})))
  (is (= {:- {[:a] 1} :+ {[:a] 2 [:b] 2 [:c] 3}}
         (->extend {:a 1} {:a 2 :b 2 :c 3})))
  (is (= {:+ {[:a] {:a 1}}} (->extend {} {:a {:a 1}})))
  (is (= {:+ {[:a] {:a 1}}} (->extend {:b 2} {:a {:a 1}})))
  (is (= {:- {[:a] 2}
          :+ {[:a] {:a 1}
              [:b] 3}}
         (->extend {:a 2} {:a {:a 1}
                           :b 3})))
  (is (= {:+ {[:b] 3}}
         (->extend {:a {:a 1}} {:a {:a 1}
                                :b 3})))
  (is (= {:+ {[:a :a] 2
              [:b]    3}
          :- {[:a :a] 1}}
         (->extend {:a {:a 1}} {:a {:a 2}
                                :b 3})))
  (is (= {:- {[:b] [1 2 3 4 5 6]}
          :+ {[:b] [4 5 6]}}
         (->extend {:b [1 2 3 4 5 6]} {:b [4 5 6]}))))


(deftest step-two
  (is (= {:- {[:a] "a"}} (->remove {} {:a "a"} {})))
  (is (= {} (->remove {} {:a "c"} {:a "a"})))
  (is (= {:- {[:b] 2}} (->remove {} {:a "c" :b 2} {:a "a"})))
  (is (= {:- {[:a] {:a 1}}} (->remove {} {:a {:a 1}} {})))
  (is (= {} (->remove {} {:a {:a {:b 5}}} {:a 2}))))


(deftest full-diff
  (is (= {} (make {} {})))
  (is (= {} (make {:a 1} {:a 1})))
  (is (= {:+ {[:a] 2}} (make {} {:a 2})))
  (is (= {:+ {[:b] 2, [:c] 3}, :- {[:a] 1}} (make {:a 1} {:b 2 :c 3})))
  (is (= {:- {[:a] 1} :+ {[:a] 2 [:b] 2 [:c] 3}}
         (make {:a 1} {:a 2 :b 2 :c 3})))
  (is (= {:+ {[:a] {:a 1}}} (make {} {:a {:a 1}})))
  (is (= {:+ {[:a] {:a 1}}, :- {[:b] 2}} (make {:b 2} {:a {:a 1}})))
  (is (= {:- {[:a] 2}
          :+ {[:a] {:a 1}
              [:b] 3}}
         (make {:a 2} {:a {:a 1}
                       :b 3})))
  (is (= {:+ {[:b] 3}
          :- {[:c] 3}}
         (make {:a {:a 1} :c 3} {:a {:a 1}
                                 :b 3})))
  (is (= {:+ {[:a :a] 2
              [:b]    3}
          :- {[:a :a] 1
              [:d]    {:e 5}}}
         (make {:a {:a 1
                    :b 2}
                :d {:e 5}}
               {:a {:a 2
                    :b 2}
                :b 3})))
  (is (= {:- {[:b] [1 2 3 4 5 6]
              [:a] "apple"}
          :+ {[:b] [4 5 6]}}
         (make {:a "apple"
                :b [1 2 3 4 5 6]} {:b [4 5 6]}))))
