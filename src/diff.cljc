(ns diff)
(ns diff
  (:require [datascript.core :as ds]
            [datascript.db :refer [db-from-reader]]
            [clojure.test :refer [deftest is]]
            [matcher-combinators.test :refer [match?]]
            [clojure.edn :as edn]))

(defn make-selector [selector value-want]
  (cond (map? value-want)  (reduce (fn [selector [k v]]
                                     (cond (map? v)  (conj selector (make-selector [k] v))
                                           (coll? v) (conj selector (make-selector [k] v))
                                           :else     (conj selector (make-selector selector  k))))
                                   selector
                                   value-want)
        (coll? value-want) (let [vec-acc       (count value-want)
                                 range-vec-acc (range vec-acc)]

                             (if (empty? range-vec-acc)
                               (conj selector [0])
                               (reduce (fn [selector v]
                                         (let [vec-val  (nth value-want v)
                                               next-sel (make-selector selector v)]
                                           (cond (coll? vec-val)  (conj selector
                                                                        (make-selector next-sel vec-val))
                                                 (coll? next-sel) (conj selector next-sel)
                                                 :else            (do (prn [next-sel])
                                                                      (conj selector next-sel)))))
                                       selector
                                       range-vec-acc)))
        :else (cond (empty? selector) (conj selector
                                            value-want)
                    :else             [value-want])))

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
  (is (match? [[:a [0 [:b]] [1 [:c]]]] (make-selector [] {:a [{:b 1}
                                                              {:c 2}]}))))

(defn map-differences [have want]
  (map make-selector have))

(deftest map-differences-test
  (is (match? {::have {}
               ::want {}
               ::have->want []})
      (map-differences {} {}))
  (is (match? {::have {:a 1}
               ::want {:a 2}
               ::have->want [{::path [:a]
                              ::have 1
                              ::want 2}]})
      (map-differences {:a 1} {:a 2}))

  (is (match? {::have {:a 1}
               ::want {:a [1]}
               ::have->want [{::path [:a]
                              ::have 1
                              ::want [1]}
                             {::path [:a 0]
                              ::have nil
                              ::want 1}]}
              (map-differences {:a 1} {:a [1]})))
  (is (match? {::have {:a 1}
               ::want {:b 2}
               ::have->want [{::path [:a]
                              ::have 1
                              ::want nil}]}
              (map-differences {:a 1} {:b 2})))
  (is (match? {::have {:a 1}
               ::want {:b 2}
               ::have->want [{::path [:a]
                              ::have 1
                              ::want nil}]}))
  (is (match? {::have {:a 1}
               ::want {:b 2}
               ::have->want [{::path [:a]
                              ::have {:b 2}
                              ::want 1}
                             {::path [:a :b]
                              ::have 2
                              ::want nil}]}
              (map-differences {:a {:b 2}} {:a 1}))))

(comment
  (require '[clojure.java.io :as io])
  (let [{:keys [schema] :as m} (db-from-reader (edn/read-string (slurp (io/resource "db.edn"))))]
    #_(ds/q '[:find (pull ?e [{:block/children
                               [{:block/children [*]}]}])
              :where [?e :block/uid "cpgzSMTye"]]
            m)
    schema
    #_(ds/conn-from-db m))
  )
