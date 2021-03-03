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
    (concat (keep (fn [df]
                    {:path     df
                     :actual (get-in want df)
                     :mismatch :-}) diff-want-paths)
            (when-not (empty? (first diff-have-paths))
              (keep (fn [df]
                      {:path     df
                       :expected (get-in have df)
                       :mismatch :+}) diff-have-paths))
            (keep (fn [eq]
                    (let [actual (get-in have eq)
                          expected (get-in want eq)]
                      (when-not (= expected actual)
                        {:path     eq
                         :actual expected
                         :expected   actual
                         :mismatch :diff}))) eq-paths))))
