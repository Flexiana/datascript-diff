(ns map-diff)

(defn- seq-diff
  [a b]
  ((resolve 'seq-diff/seq-diff) a b))

(defn- seq-commit
  [a diff]
  ((resolve 'seq-diff/seq-commit) a diff))

(defn- get-into
  "helper for handling embedded maps"
  [in-to base where what]
  (reduce (fn [old [ks w]] (assoc-in old [where (into base ks)] w)) in-to what))

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

(defn- prep_1
  [into op what]
  (reduce (fn [acc [ks v]]
            (assoc-in acc ks {op v})) into what))

(defn- prep_2
  [into op what]
  (reduce (fn [acc [ks v]]
            (update-in acc ks assoc op v)) into what))

(defn prepare-print
  [a {:keys [+ -]}]
  (-> (prep_1 a :- -)
      (prep_2 :+ +)))

(defn map-diff
  "Generates a git like diff from two maps."
  [a b]
  (let [diff (-> (expansion a b)
                 (narrowing a b))]
    (assoc diff :to-print (merge (prepare-print a diff) (:to-print diff)))
    diff))

(defn logit
  ([m x]
   (println m x)
   x)
  ([x]
   (println x)
   x))

(defn- s-or-v?
  [e]
  (and (not (map? e)) (coll? e)))

(defn- commit-sequences
  [a-map diff]
  (let [ks (distinct (concat (map vector (keys a-map)) (keys (:+ diff)) (keys (:- diff))))
        pv (:+ diff)
        mv (:- diff)]
    (if (empty? ks)
      a-map
      (reduce (fn [a k]
                (let [av (get-in a-map k)
                      p (get pv k)
                      m (get mv k)]
                  (cond
                    (nil? av) a
                    (and (s-or-v? av) (every? s-or-v? [p m])) (assoc-in a k (logit "sq" (seq-commit av (logit "sq diff: " {:+ p :- m}))))
                    :else (assoc-in a k av))))
        {} ks))))
;{:+ {[:a3] 1},
; :- {[:z] [1 1 2 3 5 7 10 18 {:a b} 19 13],
;     [:x] {:_ 2, :a b, :b [1 2 3]}},
; :to-print {:z {:- [1 1 2 3 5 7 10 18 {:a b} 19 13]}, :x {:- {:_ 2, :a b, :b [1 2 3]}}, :a3 {:+ 1}}}


(defn- reduct
  [a [ks v]]
  (let [current (first ks)]
    (cond
      (empty? ks) a
      (= 1 (count ks)) (if (s-or-v? v)
                         a
                         (dissoc a current))
      :else (assoc a current (reduct (get a current) [(rest ks) v])))))

(defn reduct2
  [a-map {:keys [- +] :as diff}]
  (reduce (fn [a [ks m]]
            (let [current (first ks)
                  p (get-in diff [:+ ks])]
              (cond
                (empty? ks) a
                (every? nil? [m p]) a
                (= 1 (count ks)) (if (and m (nil? p))
                                   (dissoc a current)
                                   a)
                :else (assoc a current (reduct2 (get a current) {:- {(rest ks) m} :+ {(rest ks) p}})))))
    a-map -))

(defn map-commit
  "Applies a diff to a map"
  [a-map {:keys [+ -] :as diff}]
  (let [sup (reduct2 a-map diff)]
    (-> (reduce (fn [a [ks v]]
                  (if (or (seq? v) (vector? v))
                    a
                    (assoc-in a ks v))) sup +)
        (commit-sequences diff))))
