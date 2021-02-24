(ns diff-test
  (:require [diff :refer [unwrap-selector make-selector #_apply-selector]]
            #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer [deftest is] :include-macros true])
            [matcher-combinators.test :refer [match?]]))

(deftest make-selector-test
  (is (match? [] (make-selector [] {})))
  (is (match? [[:a]] (make-selector [] {:a 1})))
  (is (match? [[:a] [:b]] (make-selector [] {:a 1
                                             :b 2})))
  (is (match? [[:a [:b]]] (make-selector [] {:a {:b {}}})))
  (is (match? [[:a] [:b [:c]] [:c]] (make-selector [] {:a 1
                                                       :b {:c 2}
                                                       :c 3})))
  (is (match? [[:a]
               [:b [:c
                    [:a]
                    [:d]]]
               [:c]]
              (make-selector [] {:a 1
                                 :b {:c {:a 1
                                         :d 3}}
                                 :c 3})))
  (is (match? [[0]] (make-selector [] [])))
  (is (match? [[:a [0]]] (make-selector [] {:a []})))
  (is (match? [[:a [0]]
               [:b [0]]] (make-selector [] {:a []
                                            :b []})))
  (is (match? [[0]] (make-selector [] [1])))
  (is (match? [[:a [0]]] (make-selector [] {:a [1]})))
  (is (match? [[:a [0]] [:b]] (make-selector [] {:a [1]
                                                 :b 2})))
  (is (match? [[:a
                [0]
                [1]]] (make-selector [] {:a [1 2]})))
  (is (match? [[:a
                [0]
                [1 [0]]]] (make-selector [] {:a [1 []]})))
  (is (match? [[:a
                [0 [:b]]
                [1 [:c]]]] (make-selector [] {:a [{:b 1}
                                                  {:c 2}]})))
  (is (match? [[:a [0
                    [:b
                     [0]
                     [1 [0]]]
                    [:c
                     [0 [:d]]]]]]
              (make-selector [] {:a [{:b [1 [2]]
                                      :c [{:d 1}]}]}))))

(deftest unwrap-selector-test
  (is (match? [[:a]]
              (unwrap-selector {:a 1})))
  (is (match? [[:a]
               [:b]] (unwrap-selector {:a 1
                                       :b 2})))
  (is (match? [[:a :b]]
              (unwrap-selector {:a {:b {}}})))
  (is (match? [[:a]
               [:b :c]
               [:c]]
              (unwrap-selector {:a 1
                                :b {:c 2}
                                :c 3})))
  (is (match? [[:a]
               [:b :c :a]
               [:b :c :d]
               [:c]]
              (unwrap-selector [[:a]
                                [:b [:c
                                     [:a]
                                     [:d]]]
                                [:c]])))
  (is (match? [[0]] (unwrap-selector [])))
  (is (match? [[:a 0]] (unwrap-selector [[:a [0]]])))
  (is (match? [[:a 0]
               [:b 0]] (unwrap-selector [] [[:a [0]] [:b [0]]])))
  (is (match? [[0]] (unwrap-selector [[0]])))
  (is (match? [[:a 0]] (unwrap-selector [[:a [0]]])))
  (is (match? [[:a 0]
               [:b]] (unwrap-selector [[:a [0]] [:b]])))
  (is (match? [[:a 0]
               [:a 1]] (unwrap-selector [[:a [0] [1]]])))
  (is (match? [[:a 0]
               [:a 1 0]] (unwrap-selector [[:a
                                            [0]
                                            [1 [0]]]])))
  (is (match? [[:a
                [0 [:b]]
                [1 [:c]]]] (unwrap-selector [[:a [0 [:b]] [1 [:c]]]])))
  (is (match? [[:a 0 :b 0]
               [:a 0 :b 1 0]
               [:a 0 :c 0 :d]]
              (unwrap-selector [[:a
                                 [0
                                  [:b
                                   [0]
                                   [1 [0]]]
                                  [:c
                                   [0 [:d]]]]]]))))


#_(deftest apply-selector-test
    (is (match? [{:path  [:a]
                  :value 1}]
                (apply-selector {:a 1})))
    (is (match? [{:path  [:a]
                  :value 1}
                 {:path  [:b]
                  :value 2}] (apply-selector {:a 1
                                              :b 2})))
    (is (match? [{:path  [:a :b]
                  :value nil}]
                (apply-selector {:a {:b {}}})))
    (is (match? [{:path  [:a]
                  :value 1}
                 {:path  [:b :c]
                  :value 2}
                 {:path  [:c]
                  :value 3}]
                (apply-selector {:a 1
                                 :b {:c 2}
                                 :c 3}))))
