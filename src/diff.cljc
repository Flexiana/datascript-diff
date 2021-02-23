(ns diff)
(ns diff
  (:require [datascript.core :as ds]
            [datascript.db :refer [db-from-reader]]
            [clojure.test :refer [deftest is]]
            [matcher-combinators.test :refer [match?]]
            [clojure.edn :as edn]))

(defn make-selector [selector value-want]
  (cond (map? value-want)         (reduce (fn [selector [k v :as m]]
                                            (cond (map? v)  (conj selector (make-selector [k] v))
                                                  (coll? v) (conj selector (make-selector [k] v))
                                                  :else     (conj selector (make-selector selector  k))))
                                          selector
                                          value-want)
        (and (coll? value-want)
             (empty? value-want)) (conj selector [0])
        (coll? value-want)        (let [vec-acc (count value-want)]
                                    (reduce (fn [selector v]
                                              (prn selector)
                                              (cond
                                                :else (conj selector
                                                            (make-selector selector v))))
                                            selector
                                            (range vec-acc)))
        :else                     [value-want]))

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
  (is (match? [[0]] (make-selector [] [1])))
  (is (match? [[:a [0]]] (make-selector [] {:a []})))
  (is (match? [[:a [0]]
               [:b [0]]] (make-selector [] {:a []
                                            :b []})))
  (is (match? [[:a [0]]] (make-selector [] {:a [1]})))
  (is (match? [[:a [0]] [:b]] (make-selector [] {:a [1]
                                                 :b 2})))
  (is (match? [[:a
                [0 1]]] (make-selector [] {:a [1 2]})))
  (is (match? [[:a
                [0 [1 0]]]] (make-selector [] {:a [1 []]})))
  #_(is (match? [[:a [0
                      [:b]
                      [:c]]]] (make-selector [] {:a [{:b 1}
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
    #_(ds/conn-from-db m)))

(defn get-into
  "helper for handling embedded maps"
  [in-to base where what]
  (reduce (fn [old [ks w]] (assoc-in old [where (into base ks)] w)) in-to what))

(defn expansion
  "Collects what has been added, or modified"
  [a b]
  (reduce (fn [acc [k v]]
            (let [vector-key (if (coll? k) k [k])
                  a-value (get-in a vector-key)]
              (cond
                (and (map? a-value) (map? v)) (merge acc (let [{:keys [+ -]} (expansion a-value v)]
                                                           (-> (get-into acc vector-key :- -)
                                                               (get-into vector-key :+ +))))
                (nil? a-value) (assoc-in acc [:+ vector-key] v)
                (not= v a-value) (-> (assoc-in acc [:- vector-key] a-value)
                                     (assoc-in [:+ vector-key] v))
                :else acc))) {} b))

(defn narrowing
  "Collects what has been deleted. Depending on 'fn expansion'"
  [acc a b]
  (reduce (fn [acc [k v]]
            (let [vector-key (if (coll? k) k [k])
                  b-value (get-in b vector-key)]
              (cond
                (and (map? b-value) (map? v)) (->> (narrowing {} v b-value)
                                                   :-
                                                   (get-into acc vector-key :-))
                (nil? b-value) (assoc-in acc [:- vector-key] v)
                :else acc)))
    acc
    a))

(defn make
  "Generates a git like diff from two maps."
  [a b]
  (-> (expansion a b)
      (narrowing a b)))

(defn- reduct
  [a [ks v]]
  (let [current (first ks)]
    (cond
      (empty? ks) a
      (= 1 (count ks)) (dissoc a current)
      :else (assoc a current (reduct (get a current) [(rest ks) v])))))

(defn commit
  "Applies a diff to a map"
  [a {:keys [+ -]}]
  (let [sup (reduce reduct a -)]
    (reduce (fn [a [ks v]] (assoc-in a ks v)) sup +)))

(defn- prep_1
  [into op what]
  (reduce (fn [acc [ks v]]
            (assoc-in acc ks {op v})) into what))

(defn- prep_2
  [into op what]
  (reduce (fn [acc [ks v]]
            (update-in acc ks assoc op v)) into what))

(defn prepare-print
  [a {:keys [+ -]}]
  (-> (prep_1 a :- -)
      (prep_2 :+ +)))


