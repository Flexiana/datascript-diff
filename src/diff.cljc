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
    {:idx-have idx-have
     :idx-want idx-want
     :diffs    (concat (keep (fn [df]
                               {:path     df
                                :expected (get-in want df)
                                :mismatch :+}) diff-want-paths)
                       (when-not (empty? (first diff-have-paths))
                         (keep (fn [df]
                                 {:path     df
                                  :actual   (get-in have df)
                                  :mismatch :-}) diff-have-paths))
                       (keep (fn [eq]
                               (let [expected (get-in want eq)
                                     actual   (get-in have eq)]
                                 (when-not (= expected actual)
                                   {:path     eq
                                    :actual   actual
                                    :expected expected
                                    :mismatch :diff}))) eq-paths))}))

(defn gen-uuid []
  #?(:clj (java.util.UUID/randomUUID)
     :cljs (random-uuid)))
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

(defn paths-range-to-last-idx [want-path all-paths]
  (let [unique-paths (set all-paths)]
    (when (contains? unique-paths want-path)
      (let [path-max-idx  (last want-path)
            gt-idx        (->> unique-paths
                               (filter #(= (count %)
                                           (->> unique-paths
                                                (map count)
                                                (apply max))))
                               (map last)
                               (apply max))
            is-last-path? (= gt-idx path-max-idx)]
        (if (and (>= (count want-path) 2)
                 (not is-last-path?))
          (map #(conj (pop want-path) %) (reverse (range path-max-idx (inc gt-idx))))
          want-path)))))

(defn dissoc-in
  ([m ks]
   (if-let [[k & ks] (seq ks)]
     (if (seq ks)
       (let [v (dissoc-in (get m k) ks)]
         (if (empty? v)
           (dissoc m k)
           (assoc m k v)))
       (cond (map? m)  (dissoc m k)
             (coll? m) (remove-idx k m)))
     m))
  ([m ks & kss]
   (if-let [[ks' & kss] (seq kss)]
     (recur (dissoc-in m ks) ks' kss)
     (dissoc-in m ks))))

(defn diffrange+ [path diffs]
  (if (number? (last path))
    (filter #(and (number?  (-> %
                                :path
                                last))
                  (>= (last path) (-> %
                                      :path
                                      last))) diffs)
    diffs))


(defn commit+ [{:keys [have-map want-map] :as st}
               diffs]
  (cond
    (and (nil? have-map)
         (map? want-map))  (reduce (fn [acc {:keys [expected path]}]
                                     (assoc-in acc path expected)) {} diffs)
    (and (nil? have-map)
         (coll? want-map)) (->> diffs
                                (reduce  (fn [acc {:keys [expected path]}]
                                           (assoc-in acc  path expected))
                                         {})
                                seq
                                vals
                                (apply vector))
    :else                  (reduce (fn [acc {:keys [expected path]}]
                                     (assoc-in acc path expected)) have-map diffs)))

(defn diffrange- [path diffs]
  (if (number? (last path))
    (filter #(<= (last path) (-> % :path last)) diffs)
    diffs))

(defn commit- [{:keys [have-map want-map] :as st}  diffs]
  (reduce (fn [acc {:keys [path]}]
            (dissoc-in acc path)) have-map diffs))

(defn commit-diff [{:keys [have-map want-map] :as st}
                   diffs
                   {:keys [path expected actual mismatch] :as diff}]
  (case mismatch
    :+    (let [diffs+ (diffrange+ path diffs)]
            (-> st
                (assoc :have-map (commit+ st diffs+))
                (update :txs concat (map #(assoc % :id (gen-uuid)) diffs+))))
    :-    (let [diffs- (diffrange- path diffs)]
            (-> st
                (assoc :have-map (commit- st diffs-))
                (update :txs concat (map #(assoc % :id (gen-uuid)) diffs-))))
    :diff (-> st
              (update :have-map #(assoc-in % path expected))
              (update :txs concat (map #(assoc % :id (gen-uuid)) diffs)))))

(defn uncommit [txs id have-map-st]
  (let [{:keys [path expected actual mismatch]} (first (filter #(= id (% :id)) txs))]
    (case mismatch
      :diff (assoc-in have-map-st path actual)
      :+    (dissoc-in have-map-st path)
      :-    (assoc-in have-map-st path actual))))
