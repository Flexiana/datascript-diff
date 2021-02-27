(ns diff)
(ns diff
  (:require [datascript.core :as ds]
            [datascript.db :refer [db-from-reader]]
            [clojure.test :refer [deftest is]]
            [matcher-combinators.test :refer [match?]]
            [clojure.edn :as edn]))

(defn all-paths-aux [table path]
  (letfn [(hash-f [[key val]] (all-paths-aux val (conj path key)))
          (vect-f [idx val] (all-paths-aux val (conj path idx)))]
    (cond
      (and (map? table)    (not (empty? table))) (into [] (apply concat (map hash-f table)))
      (and (vector? table) (not (empty? table))) (into [] (apply concat (map-indexed vect-f table)))
      :else                                      [path])))

(defn all-paths [table]
  (all-paths-aux table []))



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

