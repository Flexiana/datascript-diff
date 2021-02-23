(ns diff-test
  (:require [diff :refer [make-selector apply-selector]]
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

(deftest apply-selector
  (is (match? [{::path  [:a]
                ::value 1}]
              (apply-selector {:a 1})))
  (is (match? [{::path  [:a]
                ::value 1}
               {::path  [:b]
                ::value 2}] (apply-selector {:a 1
                                             :b 2})))
  (is (match? [{:path  [:a :b]
                :value nil}]
              (apply-selector {:a {:b {}}})))
  (is (match? [{::path  [:a]
                ::value 1}
               {::path [:b :c]
                :value 2}
               {::path  [:c]
                ::value 3}]
              (apply-selector {:a 1
                               :b {:c 2}
                               :c 3}))))
