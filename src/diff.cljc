(ns diff
  (:require
    [clojure.edn :as edn]
    [clojure.test :refer [deftest testing is]]
    [datascript.core :as ds]
    [datascript.db :refer [db-from-reader]]
    [matcher-combinators.test :refer [match?]]))


(comment
  (require '[clojure.java.io :as io])
  (let [{:keys [schema] :as m} (db-from-reader (edn/read-string (slurp (io/resource "db.edn"))))]
    #_(ds/q '[:find (pull ?e [{:block/children
                               [{:block/children [*]}]}])
              :where [?e :block/uid "cpgzSMTye"]]
            m)
    schema
    #_(ds/conn-from-db m)))


(defn ->extend
  [a b]
  (reduce (fn [acc [k v]]
            (let [vector-key (if (coll? k) k [k])
                  a-value (get-in a vector-key)]
              (cond
                (and (map? a-value) (map? v)) (merge acc (let [{:keys [+ -]} (->extend a-value v)
                                                               p (reduce (fn [acc [k w]]
                                                                           (assoc-in acc [:+ (into vector-key k)] w)) acc +)]
                                                           (reduce (fn [acc [k w]] (assoc-in acc [:- (into vector-key k)] w)) p -)))
                (nil? a-value) (assoc-in acc [:+ vector-key] v)
                (not= v a-value) (-> (assoc-in acc [:- vector-key] a-value)
                                     (assoc-in [:+ vector-key] v))
                :else acc))) {} b))


(defn ->remove
  [acc a b]
  (reduce (fn [acc [k v]]
            (let [vector-key (if (coll? k) k [k])
                  b-value (get-in b vector-key)]
              (cond
                (and (map? b-value) (map? v)) (merge acc (->remove acc v b-value))
                (nil? b-value) (assoc-in acc [:- vector-key] v)
                :else acc)))
          acc
          a))


(defn make
  [a b]
  (-> (->extend a b)
      (->remove a b)))
