(ns map-diff)

(defn- seq-diff
  [a b]
  ((resolve 'seq-diff/seq-diff) a b))

(defn- seq-commit
  [a diff]
  ((resolve 'seq-diff/seq-commit) a diff))

(defn- seq-revert-diff
  [diff]
  ((resolve 'seq-diff/seq-revert-diff) diff))

(defn not-map-but-coll?
  [x]
  (and (not (map? x)) (coll? x)))

(defn expansion
  "Collects what has been added, or modified"
  [a b]
  (reduce (fn [acc [k v]]
            (let [vector-key [k]
                  a-value (get-in a vector-key)]
              (cond

                (every? map? [a-value v]) (reduce (fn [acc [k v]] (assoc acc (concat vector-key k) v)) acc (expansion a-value v))
                (every? not-map-but-coll? [a-value v]) (assoc acc vector-key (seq-diff a-value v))
                (nil? a-value) (assoc acc vector-key {:+ v})
                (not= v a-value) (-> (assoc acc vector-key {:- a-value :+ v}))
                :else acc))) {} b))

(defn narrowing
  "Collects what has been deleted. Run 'fn expansion' first"
  [acc a b]
  (reduce (fn core
            [acc [k a-value]]
            (let [vector-key [k]
                  b-value (get-in b vector-key)]
              (cond
                (every? map? [b-value a-value]) (let [d (narrowing {} a-value b-value)]
                                                  (if (empty? d)
                                                    acc
                                                    (reduce (fn [acc [k v]] (assoc acc (concat vector-key k) v)) acc d)))
                (nil? b-value) (assoc-in acc [vector-key] {:- a-value})
                :else acc)))
    acc
    a))

(defn map-diff
  "Generates a git like diff from two maps."
  [a b]
  {:pre [(every? map? [a b])]}
  (-> (expansion a b)
      (narrowing a b)))

(defn map-commit
  "Applies a diff to a map"
  [a-map diff]
  {:pre [(every? map? [a-map diff])]}
  (reduce (fn core
            [acc [ks v]]
            (cond
              (empty? ks) acc
              (next ks) (assoc acc (first ks) (reduce core (get acc (first ks)) {(rest ks) v}))
              :else (let [pv (:+ v)
                          mv (:- v)
                          ov (get-in acc ks)]
                      (cond
                        pv (assoc-in acc ks pv)
                        mv (dissoc acc (first ks))
                        (every? map? [ov v]) (assoc-in acc ks (map-commit ov v))
                        (every? not-map-but-coll? [ov v]) (assoc-in acc ks (seq-commit ov v))))))
    a-map diff))

(defn- map-revert-diff
  [diff]
  (->> (map (fn [[ks v]]
              (let [pv (:+ v)
                    mv (:- v)]
                (cond
                  (or pv mv) (cond-> {}
                               pv (assoc ks {:- pv})
                               mv (assoc ks {:+ mv}))
                  (map? v) {ks (map-revert-diff v)}
                  (coll? v) {ks (seq-revert-diff v)}
                  :else v)))
            diff)
       (into {})))

(defn map-revert
  "Revert a diff on map"
  [b-map diff]
  {:pre [(every? map? [b-map diff])]}
  (->> (map-revert-diff diff)
       (map-commit b-map)))
