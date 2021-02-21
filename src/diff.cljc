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


(defn logit
  [x]
  (println x)
  x)


(defn to-add
  [a b]
  (reduce (fn [acc [k v]]
            (let [vk (if (coll? k) k [k])
                  av (get-in a vk)]
              (cond
                (and (map? av) (map? v)) (let [{:keys [+ -]} (to-add av v)
                                               p (reduce (fn [acc [k w]]
                                                           (-> (assoc-in acc [:+ (into vk k)] w)
                                                               (assoc-in  [:tmp (into vk k)] w))) acc +)]
                                           (reduce (fn [acc [k w]] (assoc-in acc [:- (into vk k)] w)) p -))

                (nil? av) (assoc-in acc [:+ vk] v)
                (not= v av) (-> (assoc-in acc [:- vk] av)
                                (assoc-in [:+ vk] v))
                :else acc))) {:tmp a} b))


(defn to-delete
  [a b]
  (reduce (fn [acc [k _]]
            (if (get b k)
              acc
              (conj acc k))) [] a))


(defn make-it
  [a b]
  (-> (to-add a b)
      (to-delete b)))


