(ns diff
  (:require [datascript.core :as ds]
            [datascript.db :refer [db-from-reader]]
            [clojure.edn :as edn]
            [clojure.set :refer [difference
                                 intersection]]))

(defn remove-idx
  ([i]
   (keep-indexed
    (fn [idx el]
      (when-not (== i idx)
        el))))
  ([i coll]
   (into []
         (remove-idx i)
         coll)))

(defn remove-idxs
  ([coll is]
   (if-let [[i & is] (seq is)]
     (if (seq is)
       (let [v (remove-idxs (nth coll i) is)]
         (if (empty? v)
           (remove-idx i v)
           (assoc coll i v)))
       (remove-idx i coll))
     coll))
  ([coll is & iss]
   (if-let [[is' & iss] (seq iss)]
     (recur (remove-idxs coll is) is' iss)
     (remove-idxs coll is))))

(defn dissoc-in
  ([m ks]
   (if-let [[k & ks] (seq ks)]
     (if (seq ks)
       (let [v (dissoc-in (get m k) ks)]
         (if (empty? v)
           (dissoc m k)
           (assoc m k v)))
       (dissoc m k))
     m))
  ([m ks & kss]
   (if-let [[ks' & kss] (seq kss)]
     (recur (dissoc-in m ks) ks' kss)
     (dissoc-in m ks))))
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

(defn remove-range-to-last-idx [want-path have-map-st]
  (let [paths (set (all-paths have-map-st))]
    (prn paths)
    (when-not (contains? paths want-path)
      (let [path-max-idx (last want-path)
            gt-idx       (apply max (map last (filter #(= (count %) (apply max (map count paths))) paths)))]
        (if-not (= gt-idx path-max-idx)
          (map #(conj (pop want-path) %) (range 1 (inc gt-idx)))
          want-path)))))
(defn map-diff [have want]
  (let [idx-have        (set (all-paths have))
        idx-want        (set (all-paths want))
        eq-paths        (intersection idx-want idx-have)
        diff-want-paths (difference idx-want idx-have)
        diff-have-paths (difference idx-have idx-want)]
    (concat (keep (fn [df]
                    {:path     df
                     :actual   (get-in want df)
                     :mismatch :+}) diff-want-paths)
            (when-not (empty? (first diff-have-paths))
              (keep (fn [df]
                      {:path     df
                       :expected (get-in have df)
                       :mismatch :-}) diff-have-paths))
            (keep (fn [eq]
                    (let [actual   (get-in want eq)
                          expected (get-in have eq)]
                      (when-not (= expected actual)
                        {:path     eq
                         :actual   expected
                         :expected actual
                         :mismatch :diff}))) eq-paths))))
