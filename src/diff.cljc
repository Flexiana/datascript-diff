(ns diff)
(ns diff
  (:require [datascript.core :as ds]
            [datascript.db :refer [db-from-reader]]
            [clojure.test :refer [deftest is]]
            [matcher-combinators.test :refer [match?]]
            [clojure.edn :as edn]))
(defn make-selector [selector value]
  (case value
    map? (map (fn [[k _v]] (make-selector selector k)) value)
    coll? (-> value)
    :else (conj selector value)))

(deftest make-selector-test
  (is (match? [] (make-selector [] {} {})))
  (is (match? [[:a]] (make-selector [] {} {:a 1})))
  (is (match? [[:a] [:b]] (make-selector [] {} {:a 1
                                                :b 2})))
  (is (match? [[:a [:b]]] (make-selector [] {} {:a {:b {}}})))
  (is (match? [[:a] [:b [:c]] [:c]] (make-selector [] {} {:a 1
                                                          :b {:c 2}
                                                          :c 3})))
  (is (match? [[:a]
               [:b [:c
                    [:a]
                    [:d]]]
               [:c]]
              (make-selector [] {} {:a 1
                                    :b {:c {:a 1
                                            :d 3}}
                                    :c 3})))
  (is (match? [[:a [0]]] {:a [1]}))
  (is (match? [[:a [0
                    [:b]
                    [:c]]]] (make-selector [] {} {:a [{:b 1}
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
