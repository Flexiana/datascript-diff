(ns seq-diff)

(defn logit
  ([m x]
   (println m x)
   x)
  ([x]
   (println x)
   x))

(defn- map-diff
  [a b]
  ((resolve 'map-diff/map-diff) a b))

(defn- map-commit
  [a b]
  ((resolve 'map-diff/map-commit) a b))

(defn get-indexes
  [v e]
  (keep-indexed (fn [idx v] (when (= e v) idx)) (vec v)))

(defn shorter
  [x y]
  (if (> (count x) (count y)) y x))

(defn longer
  [x y]
  (if (> (count (set x)) (count (set y))) x y))

(defn- glue-ordered
  [acc e]
  (if (> e (last acc))
    (concat acc [e])
    acc))

(defn- ordered-parts
  [indexes]
  (loop [idxs indexes
         acc []]
    (if (empty? idxs)
      acc
      (recur (rest idxs) (conj acc  (reduce glue-ordered [(first idxs)] (rest idxs)))))))

(ordered-parts [2 1 0 3 3])

(defn common-ordered-part
  [x y]
  (let [y-in-x (reduce concat (for [e y] (get-indexes x e)))
        x-in-y (reduce concat (for [e x] (get-indexes y e)))
        idx-y-in-x (if (empty? y-in-x)
                     []
                     (reduce longer (ordered-parts y-in-x)))
        idx-x-in-y (if (empty? x-in-y)
                     []
                     (reduce longer (ordered-parts x-in-y)))]
    (if (< (count idx-x-in-y) (count idx-y-in-x))
      (map (vec y) idx-x-in-y)
      (map (vec x) idx-y-in-x))))

(common-ordered-part [1 2 3 4] [3 1 1 2 1 3 1 4])

(defn seq-diff
  [a-seq b-seq]
  (let [diff (loop [av (vec a-seq)
                    bv (vec b-seq)
                    common (common-ordered-part a-seq b-seq)
                    acc []]
               (cond
                 (and (empty? av) (empty? bv)) acc
                 (empty? av) (concat acc (map (fn [x] {:+ x}) bv))
                 (empty? bv) (concat acc (map (fn [x] {:- x}) av))
                 (empty? common) (recur (vec (rest av)) (vec (rest bv)) [] (conj acc (cond
                                                                                       (every? map? [(first av) (first bv)]) (map-diff (first av) (first bv))
                                                                                       (map? (first av)) (map-diff (first av) {})
                                                                                       (map? (first bv)) (map-diff {} (first av))
                                                                                       :else {:- (first av) :+ (first bv)})))
                 :else (let [a-distance (.indexOf av (first common))
                             b-distance (.indexOf bv (first common))]
                         (cond
                           (< a-distance b-distance) (recur av (vec (rest bv)) common (conj acc {:+ (first bv)}))
                           (> a-distance b-distance) (recur (vec (rest av)) bv common (conj acc {:- (first av)}))
                           (= 0 a-distance b-distance) (recur (vec (rest av)) (vec (rest bv)) (vec (rest common)) (conj acc (first common)))

                           (and (= a-distance b-distance) (map? (first av)) (map? (first bv)))
                           (recur (vec (rest av)) (vec (rest bv)) common (conj acc (map-diff (first av) (first bv))))
                           (= a-distance b-distance) (recur (vec (rest av)) (vec (rest bv)) common (conj acc (if (= (first bv) (first av))
                                                                                                               (first av)
                                                                                                               {:+ (first bv) :- (first av)})))
                           :else (recur (vec (rest av)) (vec (rest bv)) (vec (rest common)) (conj acc (if (= (first bv) (first av))
                                                                                                        (first av)
                                                                                                        {:+ (first bv) :- (first av)})))))))]
    {:+        (map :+ diff)
     :-        (map :- diff)
     :to-print (map #(get % :to-print %) diff)}))

(seq-diff [:a :b :c] [:c :b :d])
;(seq-diff [1 2 3 4] [4 1 1 2 1 3 1 6])

(defn to-diff
  [e]
  (into {} (map (fn [[k v]]
                  {:+ {(vector k) (:+ v)}
                   :- {(vector k) (:- v)}}) e)))

(to-diff {:a {:- 2, :+ 3}})

(defn seq-commit
  [a-seq diff]
  #_(if (:to-print diff)
      (reduce (fn [acc e]
                (logit "e: " e)
                (conj acc (cond
                            (and (map? e) (:+ e)) (:+ e)
                            (map? e) (logit "map commit" (map-commit (logit "a: " (a-seq (count acc))) (logit (to-diff e))))
                            :else e)))

        []
        (:to-print diff)))
  (loop [as a-seq
         p (:+ diff)
         m (:- diff)
         acc []]
    (let [a (first as)
          pv (first p)
          mv (first m)]
      (cond
        ;; add all remain: as empty
        (empty? as) (concat acc (remove nil? p))
        ;; if all map? replace it with map-commit
        (every? map? [a pv mv]) (recur (rest as) (rest p) (rest m) (conj acc (map-commit a (logit "diff map" {:+ pv :- mv}))))
        ;; replace: a eq mv and pv !nil
        (and (= a mv) pv) (recur (rest as) (rest p) (rest m) (conj acc pv))
        ;; delete: a eq mv and pv nil
        (and (= a mv) (nil? pv)) (recur (rest as) (rest p) (rest m) acc)
        ;; insert: mv nil and pv !nil
        (and (nil? mv) pv) (recur as (rest p) (rest m) (conj acc pv))
        ;; keep: mv nil and pv nil
        (and (nil? mv) (nil? pv)) (recur (rest as) (rest p) (rest m) (conj acc a))))))
