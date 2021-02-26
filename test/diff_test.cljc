(ns diff-test
  (:require [diff :refer [all-paths]]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [matcher-combinators.test :refer [match?]]))

(deftest all-paths-test
  (is (match? [[]] (all-paths {})))
  (is (match? [[:a]] (all-paths {:a 1})))
  (is (match? #{[:a] [:b]} (set (all-paths {:a 1
                                            :b 2}))))
  (is (match? #{[:a :b]} (set (all-paths {:a {:b {}}}))))
  (is (match? #{[:a] [:b :c] [:c]} (set (all-paths {:a 1
                                                    :b {:c 2}
                                                    :c 3}))))
  (is (match? #{[:a]
                [:b :c]
                [:b :d]
                [:b :e]
                [:c]} (set (all-paths {:a 1
                                       :b {:c 2
                                           :d 4
                                           :e 2}
                                       :c 3}))))
  (is (match? #{[:a]
                [:b :c]
                [:d :e]
                [:f]} (set (all-paths {:a 1
                                       :b {:c 3}
                                       :d {:e 4}
                                       :f 1}))))
  (is (match? #{[:a]
                [:b :c :a]
                [:b :c :d]
                [:c]}
              (set (all-paths {:a 1
                               :b {:c {:a 1
                                       :d 3}}
                               :c 3}))))
  (is (match? #{[:a]
                [:b :c :a :f]
                [:b :c :d]
                [:c]}
              (set (all-paths {:a 1
                               :b {:c {:a {:f 1}
                                       :d 3}}
                               :c 3}))))
  (is (match? [[]] (all-paths [])))
  (is (match? [[0]] (all-paths [1])))
  (is (match? [[:a]] (all-paths {:a []})))
  (is (match? [[:a]
               [:b]] (all-paths {:a []
                                 :b []})))
  (is (match? [[:a 0]] (all-paths {:a [1]})))
  (is (match? [[:a 0] [:b]] (all-paths {:a [1]
                                        :b 2})))
  (is (match? [[:a 0]
               [:a 1]] (all-paths {:a [1 2]})))
  (is (match? [[:a 0]
               [:a 1]] (all-paths {:a [1 []]})))
  (is (match? [[:a 0 :b]
               [:a 1 :c]] (all-paths {:a [{:b 1}
                                          {:c 2}]})))
  (is (match? [[:a 0 :b 0]
               [:a 0 :b 1 0]
               [:a 0 :c 0 :d]]
              (all-paths {:a [{:b [1 [2]]
                               :c [{:d 1}]}]})))
  (is (match? [[:a]
               [:b :c]
               [:b :d]
               [:c 0 :e]
               [:c 0 :f]
               [:c 1 :e]
               [:c 1 :f]]
              (all-paths {:a 42
                          :b {:c 3 :d 4}
                          :c [{:e 1 :f 0}
                              {:e 2 :f 3}]}))))
