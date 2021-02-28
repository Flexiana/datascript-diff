(ns diff
  (:require [datascript.core :as ds]
            [datascript.db :refer [db-from-reader]]
            [clojure.edn :as edn]
            [clojure.set :refer [difference
                                 intersection]]))

(defn all-paths-aux [table path]
  (letfn [(hash-f [[key val]]
            (all-paths-aux val
                           (conj path key)))
          (vect-f [idx val]
            (all-paths-aux val
                           (conj path idx)))]
    (cond
      (and (map? table)
           (seq table)) (into [] (apply concat
                                         (map hash-f table)))
      (and (vector? table)
           (seq table)) (into [] (apply concat
                                        (map-indexed vect-f table)))
      :else             [path])))

(defn all-paths [table]
  (all-paths-aux table []))

(defn map-diff [have want]
  (let [idx-have        (set (all-paths have))
        idx-want        (set (all-paths want))
        eq-paths        (intersection idx-want idx-have)
        diff-want-paths (difference idx-want idx-have)
        diff-have-paths (difference idx-have idx-want)]
    (prn idx-have idx-want)
    (prn [diff-want-paths diff-have-paths])
    (concat (keep (fn [df]
                    {:path     df
                     :expected (get-in want df)
                     :mismatch :-}) diff-want-paths)
            (when-not (empty? (first diff-have-paths))
              (keep (fn [df]
                      {:path     df
                       :actual   (get-in have df)
                       :mismatch :+}) diff-have-paths))
            (keep (fn [eq]
                    (let [expected (get-in have eq)
                          actual   (get-in want eq)]
                      (when-not (= expected actual)
                        {:path     eq
                         :expected expected
                         :actual   actual
                         :mismatch :diff}))) eq-paths))))

(comment
  (require '[clojure.java.io :as io])
  (let [{:keys [schema] :as m} (db-from-reader (edn/read-string (slurp (io/resource "db.edn"))))
        conn                   (ds/conn-from-db m)
        {db0 :db-after}        (ds/transact! conn [{:create/user  #:db{:id 6},
                                                    :block/string "D",
                                                    :create/time  (.getTime (java.util.Date.))
                                                    :edit/user    #:db{:id 6},
                                                    :block/uid    "Yg5K4Tb3S",
                                                    :block/open   true,
                                                    :edit/time    (.getTime (java.util.Date.)),
                                                    :db/id        -1,
                                                    :block/parents
                                                    [#:db{:id 40868}
                                                     #:db{:id 40869}
                                                     #:db{:id 40879}
                                                     #:db{:id 40880}
                                                     #:db{:id 40901}
                                                     #:db{:id 40913}],
                                                    :block/order  3,
                                                    :block/page   #:db{:id 40868}}])]
    #_(sort (keys  (:schema (ds/q '[:find ?c .
                                    :in $
                                    :where
                                    [(identity $) ?c]]
                                  m))))

    #_(ds/q '[:find (pull ?e [* {:block/children ...}])
              :where
              [?e :block/uid "cpgzSMTye"]

              ]
            m)
    (ds/q '[:find (pull ?e [* {:block/children [{:block/children [*]}]}])
            :where [?e :block/uid "cpgzSMTye"]]
          db0)
    #_(ds/pull m '[*] 40920)
    #_(ds/pull m '[* #_{:block/children ...}]
               [:block/string "A"]))
  #_(let [h1                (:datoms (edn/read-string (slurp "/home/ianffcs/Downloads/hello1.txt")))
          h2                (:datoms (edn/read-string (slurp "/home/ianffcs/Downloads/hello2.txt")))
          [h1->h2 h2->h1 _] (clojure.data/diff h1 h2)]
      (remove #(do (prn %) (nil? %)) (take 20 h1->h2))))

#_(deftest crud-test
    (let [conn                                (doto (ds/create-conn tg-schema))
          {:keys [db-after tempids]}          (ds/transact! conn
                                                            [{:db/id            -1
                                                              :task/description "abc"
                                                              :task/checked     false}])
          id-abc                              (ds/resolve-tempid db-after tempids -1)
          {db-check-task :db-after}           (ds/transact! conn (check-task id-abc))
          {db-create-task  :db-after
           tempids-created :tempids}          (->> [["ola" -2] ["hello" -3]]
                                                   (mapcat (fn [[desc tempid]]
                                                             (create-task desc tempid)))
                                                   (ds/transact! conn))
          [id-ola id-hello]                   (->> [-2 -3]
                                                   (map #(ds/resolve-tempid db-after tempids-created %)))
          {db-set-description-task :db-after} (->> [[id-ola "alo"] [id-abc "ioa"]]
                                                   (mapcat (fn [[id desc]]
                                                             (set-description-task id desc)))
                                                   (ds/transact! conn))
          {db-uncheck-task :db-after}         (->> [id-hello id-abc]
                                                   (mapcat uncheck-task)
                                                   (ds/transact! conn))
          {db-delete-task :db-after}          (->> [id-hello id-abc]
                                                   (mapcat delete-task)
                                                   (ds/transact! conn))]
      (testing
          "found task abc with completed true"
        (is (match? [{:db/id            id-abc
                      :task/checked     true
                      :task/description "abc"}]
                    (tasks db-check-task))))
      (testing
          "A task ola and hello created and appearing in tasks?"
        (is (match? [{:db/id            3
                      :task/checked     false
                      :task/description "hello"}
                     {:db/id            2
                      :task/checked     false
                      :task/description "ola"}
                     {:db/id            1
                      :task/checked     true
                      :task/description "abc"}]
                    (tasks db-create-task))))

      (testing "db updating description of task 2 to alo and 1 to ioa"
        (is (match? [{:db/id 3, :task/checked false, :task/description "hello"}
                     {:db/id 2, :task/checked false, :task/description "alo"}
                     {:db/id 1, :task/checked true, :task/description "ioa"}]
                    (tasks db-set-description-task))))

      (testing "db uncomplete tasks 1 and 3"
        (is (match? [{:db/id 3, :task/checked false, :task/description "hello"}
                     {:db/id 2, :task/checked false, :task/description "alo"}
                     {:db/id 1, :task/checked false, :task/description "ioa"}]
                    (tasks db-uncheck-task))))

      (testing "delete tasks 1 and 2"
        (is (match? [{:db/id 2, :task/checked false, :task/description "alo"}]
                    (tasks db-delete-task))))))
