(ns recursive-diff-test
  (:require
    [clojure.test :refer :all]
    [recursive-diff :refer [map-commit
                            map-diff
                            narrowing
                            prepare-print
                            expansion]]))

(deftest step-one
  (is (= {} (expansion {} {})))
  (is (= {} (expansion {:a 1} {:a 1})))
  (is (= {:+ {[:a] 2}} (expansion {} {:a 2})))
  (is (= {:+ {[:b] 2 [:c] 3}} (expansion {:a 1} {:b 2 :c 3})))
  (is (= {:- {[:a] 1} :+ {[:a] 2 [:b] 2 [:c] 3}}
         (expansion {:a 1} {:a 2 :b 2 :c 3})))
  (is (= {:+ {[:a] {:a 1}}} (expansion {} {:a {:a 1}})))
  (is (= {:+ {[:a] {:a 1}}} (expansion {:b 2} {:a {:a 1}})))
  (is (= {:- {[:a] 2}
          :+ {[:a] {:a 1}
              [:b] 3}}
         (expansion {:a 2} {:a {:a 1}
                            :b 3})))
  (is (= {:+ {[:b] 3}}
         (expansion {:a {:a 1}} {:a {:a 1}
                                 :b 3})))
  (is (= {:+ {[:a :a] 2
              [:b]    3}
          :- {[:a :a] 1}}
         (expansion {:a {:a 1}} {:a {:a 2}
                                 :b 3})))
  (is (= {:- {[:b] [1 2 3 4 5 6]}
          :+ {[:b] [4 5 6]}}
         (expansion {:b [1 2 3 4 5 6]} {:b [4 5 6]}))))

(deftest step-two
  (is (= {:- {[:a] "a"}} (narrowing {} {:a "a"} {})))
  (is (= {} (narrowing {} {:a "c"} {:a "a"})))
  (is (= {:- {[:b] 2}} (narrowing {} {:a "c" :b 2} {:a "a"})))
  (is (= {:- {[:a] {:a 1}}} (narrowing {} {:a {:a 1}} {})))
  (is (= {} (narrowing {} {:a {:a {:b 5}}} {:a 2}))))

(deftest full-diff
  (is (= {} (map-diff {} {})))
  (is (= {} (map-diff {:a 1} {:a 1})))
  (is (= {:+ {[:a] 2}} (map-diff {} {:a 2})))
  (is (= {:+ {[:b] 2, [:c] 3}, :- {[:a] 1}} (map-diff {:a 1} {:b 2 :c 3})))
  (is (= {:- {[:a] 1} :+ {[:a] 2 [:b] 2 [:c] 3}}
         (map-diff {:a 1} {:a 2 :b 2 :c 3})))
  (is (= {:+ {[:a] {:a 1}}} (map-diff {} {:a {:a 1}})))
  (is (= {:+ {[:a] {:a 1}}, :- {[:b] 2}} (map-diff {:b 2} {:a {:a 1}})))
  (is (= {:- {[:a] 2}
          :+ {[:a] {:a 1}
              [:b] 3}}
         (map-diff {:a 2} {:a {:a 1}
                           :b     3})))
  (is (= {:+ {[:b] 3}
          :- {[:c] 3}}
         (map-diff {:a {:a 1} :c 3} {:a {:a 1}
                                     :b     3})))
  (is (= {:+ {[:a :a] 2
              [:b]    3}
          :- {[:a :a] 1
              [:d]    {:e 5}}}
         (map-diff {:a {:a 1
                        :b     2}
                    :d     {:e 5}}
                   {:a {:a 2
                        :b 2}
                    :b 3})))
  (is (= {:- {[:b] [1 2 3 4 5 6]
              [:a] "apple"}
          :+ {[:b] [4 5 6]}}
         (map-diff {:a "apple"
                    :b     [1 2 3 4 5 6]} {:b [4 5 6]}))))

(defn test-commit
  [a b]
  (is (= b (->> (map-diff a b)
                (map-commit a))))
  (is (= a (->> (map-diff b a)
                (map-commit b)))))

(deftest apply-diff
  (test-commit {} {})
  (test-commit {:a 1} {:a 1})
  (test-commit {} {:a 2})
  (test-commit {:a 1} {:b 2 :c 3})
  (test-commit {:a 1} {:a 2 :b 2 :c 3})
  (test-commit {} {:a {:a 1}})
  (test-commit {:b 2} {:a {:a 1}})
  (test-commit {:a 2} {:a {:a 1} :b 3})
  (test-commit {:a {:a 1} :c 3} {:a {:a 1} :b 3})
  (test-commit {:a {:a 1} :b 2 :d {:e 5}} {:a {:a 2 :b 2} :b 3})
  (test-commit {:a {:a {:a 1 :b 2}} :b 2 :d {:e 5}} {:a {:a {:a 2} :b 2} :b 3})
  (test-commit {:a "apple" :b [1 2 3 4 5 6]} {:b [4 5 6]}))

(deftest print-prepare
  (is (= {:a {:a {:- 1, :+ 2}, :b 2}, :d {:- {:e 5}}, :b {:+ 3}}
         (prepare-print {:a {:a 1
                             :b 2}
                         :d {:e 5}}
                        {:+ {[:a :a] 2
                             [:b]    3}
                         :- {[:a :a] 1
                             [:d]    {:e 5}}}))))
