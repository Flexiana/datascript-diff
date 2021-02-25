(ns map-diff)

(defn logit
  ([m x]
   (println m x)
   x)
  ([x]
   (println x)
   x))

(defn- seq-diff
  [a b]
  ((resolve 'seq-diff/seq-diff) a b))

(defn- seq-commit
  [a diff]
  ((resolve 'seq-diff/seq-commit) a diff))

(defn get-into
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
                (and (map? a-value) (map? v)) (merge acc (let [{:keys [+ -]} (expansion a-value v)]
                                                           (-> (get-into acc vector-key :- -)
                                                               (get-into vector-key :+ +))))
                (and (coll? a-value) (coll? v)) (let [{:keys [+ -]} (seq-diff a-value v)]
                                                  (-> (update-in acc [:- vector-key] concat -)
                                                      (update-in [:+ vector-key] concat +)))
                (nil? a-value) (assoc-in acc [:+ vector-key] v)
                (not= v a-value) (-> (assoc-in acc [:- vector-key] a-value)
                                     (assoc-in [:+ vector-key] v))
                :else acc))) {} b))

(defn narrowing
  "Collects what has been deleted. Run 'fn expansion' first"
  [acc a b]
  (reduce (fn [acc [k v]]
            (let [vector-key (if (coll? k) k [k])
                  b-value (get-in b vector-key)]
              (cond
                (and (map? b-value) (map? v)) (->> (narrowing {} v b-value)
                                                   :-
                                                   (get-into acc vector-key :-))
                (nil? b-value) (assoc-in acc [:- vector-key] v)
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
    (assoc diff :to-print (prepare-print a diff))))

(defn s-or-v?
  [e]
  (or (seq? e) (vector? e)))

(defn- reduct
  [a [ks v]]
  (let [current (first ks)]
    (cond
      (empty? ks) a
      (= 1 (count ks)) (if (s-or-v? v)
                         a
                         (dissoc a current))
      :else (assoc a current (reduct (get a current) [(rest ks) v])))))

(defn commit-seqs
  [a-map diff]
  (let [ks (distinct (concat (keys (:+ diff)) (keys (:- diff))))
        pv (:+ diff)
        mv (:- diff)]
    (reduce (fn [a k]
              (let [av (get-in a-map k)
                    p (get pv k)
                    m (get mv k)]
                (cond
                  (every? s-or-v? [av p m]) (assoc-in a-map k (seq-commit av {:+ p :- m}))
                  :else a)))
      a-map ks)))

(defn map-commit
  "Applies a diff to a map"
  [a-map {:keys [+ -] :as diff}]
  (let [sup (reduce reduct a-map -)]
    (-> (reduce (fn [a [ks v]]
                  (if (or (seq? v) (vector? v))
                    a
                    (assoc-in a ks v))) sup +)
        (commit-seqs diff))))
