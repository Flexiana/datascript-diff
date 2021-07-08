(ns diff-test
  (:require [diff :refer [all-paths
                          map-diff
                          remove-idxs
                          paths-range-to-last-idx
                          commit-diff]]
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
  (is (match? [] (:diffs (map-diff {} {}))))
  (is (match? [] (:diffs (map-diff {:a 1} {:a 1}))))
  (is (match? [] (:diffs (map-diff {:a {0 1
                                        1 2}}
                                   {:a [1 2]}))))

  (is (match? [{:path     [:a]
                :expected 2
                :mismatch :+}] (:diffs (map-diff {} {:a 2}))))
  (is (match? [{:path [:b], :expected 2, :mismatch :+}
               {:path [:c], :expected 3, :mismatch :+}
               {:path [:a], :actual 1, :mismatch :-}]
              (:diffs (map-diff {:a 1} {:b 2 :c 3}))))
  (is (match? [{:path [:b], :expected 2, :mismatch :+}
               {:path [:c], :expected 3, :mismatch :+}
               {:path [:a], :actual 1, :expected 2, :mismatch :diff}]
              (:diffs (map-diff {:a 1} {:a 2 :b 2 :c 3}))))

  (is (match? [{:path   [:a :a]
                :expected 1, :mismatch :+}]
              (:diffs (map-diff {} {:a {:a 1}}))))

  (is (match? [{:path [:a :a], :expected 1, :mismatch :+}
               {:path [:b], :actual 2, :mismatch :-}]
              (:diffs (map-diff {:b 2} {:a {:a 1}}))))

  (is (match? [{:path [:a :a], :expected 1, :mismatch :+}
               {:path [:b], :expected 3, :mismatch :+}
               {:path [:a], :actual 2, :mismatch :-}]
              (:diffs (map-diff {:a 2} {:a {:a 1}
                                        :b 3}))))

  (is (match? [{:path [:b], :expected 3, :mismatch :+}
               {:path [:c], :actual 3, :mismatch :-}]
              (:diffs (map-diff {:a {:a 1}
                                 :c 3} {:a {:a 1}
                                        :b 3}))))
  (is (match? [{:path [:a :b], :expected 2, :mismatch :+}
               {:path [:d :e], :actual 5, :mismatch :-}
               {:path [:a :a], :actual 1, :expected 2, :mismatch :diff}
               {:path [:b], :actual 2, :expected 3, :mismatch :diff}]
              (:diffs (map-diff {:a {:a 1}
                                 :b 2 :d {:e 5}} {:a {:a 2 :b 2} :b 3}))))
  (is (match? [{:path [:a :b], :expected 2, :mismatch :+}
               {:path [:a :a :b], :actual 2, :mismatch :-}
               {:path [:d :e], :actual 5, :mismatch :-}
               {:path [:a :a :a], :actual 1, :expected 2, :mismatch :diff}
               {:path [:b], :actual 2, :expected 3, :mismatch :diff}]
              (:diffs (map-diff {:a {:a {:a 1 :b 2}}
                                 :b 2 :d {:e 5}} {:a {:a {:a 2} :b 2} :b 3}))))
  (is (match? [{:path [:a], :actual "apple", :mismatch :-}
               {:path [:b 3], :actual 4, :mismatch :-}
               {:path [:b 4], :actual 5, :mismatch :-}
               {:path [:b 5], :actual 6, :mismatch :-}
               {:path [:b 0], :actual 1, :expected 4, :mismatch :diff}
               {:path [:b 1], :actual 2, :expected 5, :mismatch :diff}
               {:path [:b 2], :actual 3, :expected 6, :mismatch :diff}]
              (:diffs (map-diff {:a "apple"
                                 :b [1 2 3 4 5 6]} {:b [4 5 6]}))))

  (is (match? [] (:diffs (map-diff {:x [{:a 3} 2]} {:x [{:a 3} 2]}))))
  (is (match? [{:path [:a 0], :actual :c, :expected :a, :mismatch :diff}
               {:path [:a 2], :actual :d, :expected :c, :mismatch :diff}]
              (:diffs (map-diff {:a [:c :b :d]} {:a [:a :b :c]}))))
  (is (match? [{:path [:b], :expected 3, :mismatch :+}
               {:path [:d :e], :actual 5, :mismatch :-}
               {:path [:a :a], :actual 1, :expected 2, :mismatch :diff}]
              (:diffs (map-diff {:a {:a 1
                                     :b 2}
                                 :d {:e 5}} {:a {:a 2
                                                 :b 2}
                                             :b 3}))))
  (is (match? [{:path [:a 0], :expected 123, :mismatch :+}
               {:path [:a], :actual "apple", :mismatch :-}
               {:path [:b 3], :actual 4, :mismatch :-}
               {:path [:b 4], :actual 5, :mismatch :-}
               {:path [:b 5], :actual 6, :mismatch :-}
               {:path [:b 0], :actual 1, :expected 4, :mismatch :diff}
               {:path [:b 1], :actual 2, :expected 5, :mismatch :diff}
               {:path [:b 2], :actual 3, :expected 6, :mismatch :diff}]
              (:diffs
                (map-diff {:a "apple"
                           :b [1 2 3 4 5 6]} {:b [4 5 6]
                                              :a [123]})))))

(deftest remove-idxs-test
  (is (match? [2 [0 1]] (remove-idxs [2 [0 1 2]] [1 2]))))


(deftest paths-range-to-last-idx-test
  (is (match? [[3 3]
               [3 2]
               [3 1]]
              (paths-range-to-last-idx [3 1] (all-paths [1 2 3 [3 4 5 6]]))))
  (is (match? [2]
              (paths-range-to-last-idx [2] (all-paths [1 2 3 [3 4 5 6]])))))

(deftest commit-diff-test
  (is (match? {:have-map {:a 1}}
              (dissoc (commit-diff {:have-map nil
                                    :want-map {:a 1}}
                                   (:diffs (map-diff nil {:a 1}))
                                   {:path     [:a]
                                    :expected 1
                                    :mismatch :+})
                      :txs)))
  (is (match? {:have-map {:a 2}}
              (dissoc (commit-diff {:have-map {:a 1}
                                    :want-map {:a 2}}
                                   [{:path [:a], :actual 1, :expected 2, :mismatch :diff}]
                                   {:path     [:a]
                                    :actual   1
                                    :expected 2
                                    :mismatch :+})
                      :txs)))
  (is (match? {:have-map [1]}
              (dissoc (commit-diff {:have-map nil
                                    :want-map [1 2 4]}
                                   (:diffs (map-diff nil [1 2 4]))
                                   {:path     [0]
                                    :expected 1
                                    :mismatch :+})
                      :txs)))
  (is (match? {:have-map [1 2]}
              (dissoc (commit-diff {:have-map nil
                                    :want-map [1 2 4]}
                                   (:diffs (map-diff nil [1 2 4]))
                                   {:path     [1]
                                    :actual   nil
                                    :expected 2
                                    :mismatch :+})
                      :txs)))
  (is (match? {:have-map [3]}
              (dissoc (commit-diff {:have-map [4]
                                    :want-map [3]}
                                   (:diffs (map-diff nil [3]))
                                   {:path     [0]
                                    :actual   4
                                    :expected 3
                                    :mismatch :diff})
                      :txs)))
  (is (match? {:have-map [1 2]} (dissoc (commit-diff {:have-map [1 2 3 4 5]
                                                      :want-map [1 2]}
                                                     (:diffs (map-diff [1 2 3 4 5] [1 2]))
                                                     {:path     [2]
                                                      :actual   3
                                                      :mismatch :-})
                                        :txs)))
  (is (match? {:have-map {:a {0 1}}}
              (dissoc (commit-diff
                       {:have-map {:a 1}
                        :want-map {:a [1 2]}}
                       [{:path [:a 0], :expected 1, :mismatch :+}
                        {:path [:a 1], :expected 2, :mismatch :+}
                        {:path [:a], :actual 1, :mismatch :-}]
                       {:path [:a 0], :expected 1, :mismatch :+})
                      :txs)))
  (is (match? {:have-map [1 2]}
              (dissoc (commit-diff {:have-map [1 2 3 4 5]
                                    :want-map [1 2]}
                                   [{:path [2], :actual 3, :mismatch :-}
                                    {:path [3], :actual 4, :mismatch :-}
                                    {:path [4], :actual 5, :mismatch :-}]
                                   {:path [2], :actual 3, :mismatch :-})
                      :txs)))
  (is (match? {:have-map {:a 1}}
              (commit-diff {:have-map {:a {:b 2}}
                            :want-map {:a 1}}
                           (:diffs (map-diff {:a {:b 2}} {:a 1}))
                           {:path [:a], :expected 1, :mismatch :+})))
  (is (match? {:have-map {:a {0 1 1 2}}}
              (commit-diff {:have-map {:a 1}
                            :want-map {:a [1 2]}}
                           (:diffs (map-diff {:a 1} {:a [1 2]}))
                           {:path [:a 1], :expected 1, :mismatch :+})))
  (is (match? {:have-map [{:a 1}]}
              (commit-diff {:have-map [{:a 1} {:a 1}]
                            :want-map [{:a 1}]}
                           (:diffs (map-diff [{:a 1} {:a 1}] [{:a 1}]))
                           {:path [1 :a], :actual 1, :mismatch :-})))
  (is (match? {:have-map [[false] [true]]}
              (commit-diff {:have-map [[true] [false] [true] [true]]
                            :want-map []}
                           (:diffs (map-diff [[true] [false] [true] [true]] []))
                           {:path [0 0], :actual true, :mismatch :-})))
  (is (match? {:have-map [{:a 1}
                          {:a {:b {0 {:c 3}}}}]}
              (commit-diff {:have-map [{:a 1}
                                       {:a 1}]
                            :want-map [{:a 2}
                                       {:a {:b [{:c 3}
                                                [:d 4]]}}]}
                           (:diffs (map-diff
                                    [{:a 2}
                                     {:a {:b [{:c 3}
                                              [:d 4]]}}]
                                    [{:a 1}
                                     {:a 1}]))
                           {:path [1 :a :b 0 :c] :expected 3 :mismatch :+}))))
