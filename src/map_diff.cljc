(ns map-diff)

(defn- seq-diff
  [a b]
  ((resolve 'seq-diff/seq-diff) a b))

(defn- seq-commit
  [a diff]
  ((resolve 'seq-diff/seq-commit) a diff))

(defn expansion
  "Collects what has been added, or modified"
  [a b]
  (reduce (fn [acc [k v]]
            (let [vector-key (if (coll? k) k [k])
                  a-value (get-in a vector-key)]
              (cond
                (and (map? a-value) (map? v)) (reduce (fn [acc [k v]]
                                                        (assoc acc (concat vector-key k) v)) acc (expansion a-value v))
                (and (coll? a-value) (coll? v) (not= a-value v)) (assoc acc vector-key (seq-diff a-value v))
                (nil? a-value) (assoc acc vector-key {:+ v})
                (not= v a-value) (-> (assoc acc vector-key {:- a-value :+ v}))
                :else acc))) {} b))

(defn narrowing
  "Collects what has been deleted. Run 'fn expansion' first"
  [acc a b]
  (reduce (fn core
            [acc [k a-value]]
            (let [vector-key (if (coll? k) k [k])
                  b-value (get-in b vector-key)]
              (cond
                (and (map? b-value) (map? a-value)) (let [d (narrowing {} a-value b-value)]
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
  (-> (expansion a b)
      (narrowing a b)))

(defn map-commit
  "Applies a diff to a map"
  [a-map diff]
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
                        (every? coll? [ov v]) (assoc-in acc ks (seq-commit ov v))))))
    a-map diff))

