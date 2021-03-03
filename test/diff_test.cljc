(ns diff-test
  (:require [diff :refer [all-paths map-diff]]
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
                              {:e 2 :f 3}]})))
  (is (match? [[0 :a]
               [0 :b]
               [1 :d]
               [1 :c :d 0]
               [1 :c :d 1]] (all-paths [{:a 1 :b 2}
                                        {:d 2
                                         :c {:d [4 5]}}]))))



(deftest full-diff
  (is (match? [] (map-diff {} {})))
  (is (match? [] (map-diff {:a 1} {:a 1})))
  (is (match? [{:path     [:a]
                :actual 2
                :mismatch :+}] (map-diff {} {:a 2})))
  (is (match? [{:path [:b], :actual 2, :mismatch :+}
               {:path [:c], :actual 3, :mismatch :+}
               {:path [:a], :expected 1, :mismatch :-}]
              (map-diff {:a 1} {:b 2 :c 3})))
  (is (match? [{:path [:b], :actual 2, :mismatch :+}
               {:path [:c], :actual 3, :mismatch :+}
               {:path [:a], :actual 1, :expected 2, :mismatch :diff}]
              (map-diff {:a 1} {:a 2 :b 2 :c 3})))
  (is (match? [{:path     [:a :a]
                :actual 1, :mismatch :+}]
              (map-diff {} {:a {:a 1}})))
  (is (match? [{:path [:a :a], :actual 1, :mismatch :+}
               {:path [:b], :expected 2, :mismatch :-}]
              (map-diff {:b 2} {:a {:a 1}})))
  (is (match? [{:path [:b], :actual 3, :mismatch :+}
               {:path [:a :a], :actual 1, :mismatch :+}
               {:path [:a], :expected 2, :mismatch :-}]
              (map-diff {:a 2} {:a {:a 1}
                                :b 3})))
  (is (match? [{:path [:b], :actual 3, :mismatch :+}
               {:path [:c], :expected 3, :mismatch :-}]
              (map-diff {:a {:a 1}
                         :c 3} {:a {:a 1}
                                :b 3})))
  (is (match? [{:path [:a :b], :actual 2, :mismatch :+}
               {:path [:d :e], :expected 5, :mismatch :-}
               {:path [:b], :actual 2, :expected 3, :mismatch :diff}
               {:path [:a :a], :actual 1, :expected 2, :mismatch :diff}]
              (map-diff {:a {:a 1}
                         :b 2 :d {:e 5}} {:a {:a 2 :b 2} :b 3})))
  (is (match? [{:path [:a :b], :actual 2, :mismatch :+}
               {:path [:a :a :b], :expected 2, :mismatch :-}
               {:path [:d :e], :expected 5, :mismatch :-}
               {:path [:a :a :a], :actual 1, :expected 2, :mismatch :diff}
               {:path [:b], :actual 2, :expected 3, :mismatch :diff}]
              (map-diff {:a {:a {:a 1 :b 2}}
                         :b 2 :d {:e 5}} {:a {:a {:a 2} :b 2} :b 3})))
  (is (match? [{:path [:b 5], :expected 6, :mismatch :-}
               {:path [:b 4], :expected 5, :mismatch :-}
               {:path [:a], :expected "apple", :mismatch :-}
               {:path [:b 3], :expected 4, :mismatch :-}
               {:path [:b 0], :actual 1, :expected 4, :mismatch :diff}
               {:path [:b 2], :actual 3, :expected 6, :mismatch :diff}
               {:path [:b 1], :actual 2, :expected 5, :mismatch :diff}]
              (map-diff {:a "apple"
                         :b [1 2 3 4 5 6]} {:b [4 5 6]})))
  (is (match? [] (map-diff {:x [{:a 3} 2]} {:x [{:a 3} 2]})))
  (is (match? [{:path [:a 2], :actual :d, :expected :c, :mismatch :diff}
               {:path [:a 0], :actual :c, :expected :a, :mismatch :diff}]
              (map-diff {:a [:c :b :d]} {:a [:a :b :c]})))
  (is (match? [{:path [:b], :actual 3, :mismatch :+}
               {:path [:d :e], :expected 5, :mismatch :-}
               {:path [:a :a], :actual 1, :expected 2, :mismatch :diff}]
              (map-diff {:a {:a 1
                             :b 2}
                         :d {:e 5}} {:a {:a 2
                                         :b 2}
                                     :b 3})))
  (is (match? [{:path [:a 0], :actual 123, :mismatch :+}
               {:path [:b 5], :expected 6, :mismatch :-}
               {:path [:b 4], :expected 5, :mismatch :-}
               {:path [:a], :expected "apple", :mismatch :-}
               {:path [:b 3], :expected 4, :mismatch :-}
               {:path [:b 0], :actual 1, :expected 4, :mismatch :diff}
               {:path [:b 2], :actual 3, :expected 6, :mismatch :diff}
               {:path [:b 1], :actual 2, :expected 5, :mismatch :diff}]
              (map-diff {:a "apple"
                         :b [1 2 3 4 5 6]} {:b [4 5 6]
                                            :a [123]}))))
