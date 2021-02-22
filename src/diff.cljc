(ns diff
  (:require [datascript.core :as ds]
            [datascript.db :refer [db-from-reader]]
            [clojure.test :refer [deftest is]]
            [matcher-combinators.test :refer [match?]]
            [clojure.edn :as edn]))

(defn make-selector
  [selector value]
  (case value
    map? (map (fn [[k _v]] (make-selector selector k)) value)
    coll? (-> value)
    :else (conj selector value)))

(deftest make-selector-test
  (is (match? [] (make-selector [] {})))
  (is (match? [:a] (make-selector [] {:a 1})))
  (is (match? [:a 0] (make-selector [] {:a [1]}))))

(defn map-differences
  [have want]
  (map make-selector have))

(deftest map-differences-test
  (is (match? {::have       {}
               ::want       {}
               ::have->want []})
      (map-differences {} {}))
  (is (match? {::have       {:a 1}
               ::want       {:a 2}
               ::have->want [{::path [:a]
                              ::have 1
                              ::want 2}]})
      (map-differences {:a 1} {:a 2}))

  (is (match? {::have       {:a 1}
               ::want       {:a [1]}
               ::have->want [{::path [:a]
                              ::have 1
                              ::want [1]}
                             {::path [:a 0]
                              ::have nil
                              ::want 1}]}
              (map-differences {:a 1} {:a [1]})))
  (is (match? {::have       {:a 1}
               ::want       {:b 2}
               ::have->want [{::path [:a]
                              ::have 1
                              ::want nil}]}
              (map-differences {:a 1} {:b 2})))
  (is (match? {::have       {:a 1}
               ::want       {:b 2}
               ::have->want [{::path [:a]
                              ::have 1
                              ::want nil}]}))
  (is (match? {::have       {:a 1}
               ::want       {:b 2}
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
  [in-to base where what]
  (reduce (fn [old [ks w]] (assoc-in old [where (into base ks)] w)) in-to what))

(defn expansion
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
  [a b]
  (-> (expansion a b)
      (narrowing a b)))

(defn reduct
  [a [ks v]]
  (let [current (first ks)]
    (cond
      (empty? ks) a
      (= 1 (count ks)) (dissoc a current)
      :else (assoc a current (reduct (get a current) [(rest ks) v])))))

(defn commit
  [a {:keys [+ -]}]
  (let [sup (reduce reduct a -)]
    (reduce (fn [a [ks v]] (assoc-in a ks v)) sup +)))
