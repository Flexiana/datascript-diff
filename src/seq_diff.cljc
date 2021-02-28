(ns seq-diff)

(defn- map-diff
  [a b]
  ((resolve 'map-diff/map-diff) a b))

(defn- map-commit
  [a b]
  ((resolve 'map-diff/map-commit) a b))

(defn- get-indexes
  [v e]
  (keep-indexed (fn [idx v] (when (= e v) idx)) (vec v)))

(defn- shorter
  [x y]
  (if (> (count x) (count y)) y x))

(defn- longer
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
      (recur (rest idxs) (conj acc (reduce glue-ordered [(first idxs)] (rest idxs)))))))

(defn common-ordered-part
  [x y]
  (let [longest (fn [op]
                  (if (empty? op)
                    []
                    (reduce longer op)))
        ordered-part (->> (for [e y] (get-indexes x e))
                          (reduce concat)
                          ordered-parts
                          longest)]
    (map (vec x) ordered-part)))

(defn seq-diff
  [a-seq b-seq]
  (let [diff (loop [av (vec a-seq)
                    bv (vec b-seq)
                    common (common-ordered-part a-seq b-seq)
                    acc []]

               (let [a-distance (when common (.indexOf av (first common)))
                     b-distance (when common (.indexOf bv (first common)))]
                 (cond
                   (every? empty? [av bv]) acc
                   (empty? av) (concat acc (map (fn [x] {:+ x}) bv))
                   (empty? bv) (concat acc (map (fn [x] {:- x}) av))
                   (empty? common) (recur (vec (rest av)) (vec (rest bv)) [] (conj acc (cond
                                                                                         (every? map? [(first av) (first bv)]) (map-diff (first av) (first bv))
                                                                                         (every? coll? [(first av) (first bv)]) (seq-diff (first av) (first bv))
                                                                                         :else (if (= (first av) (first bv)) (first av) {:- (first av) :+ (first bv)}))))
                   (< a-distance b-distance) (recur av (vec (rest bv)) common (conj acc {:+ (first bv)}))
                   (> a-distance b-distance) (recur (vec (rest av)) bv common (conj acc {:- (first av)}))
                   (and (= a-distance b-distance) (every? map? [(first av) (first bv)])) (recur (vec (rest av)) (vec (rest bv)) common (conj acc (map-diff (first av) (first bv))))
                   (and (= a-distance b-distance) (every? coll? [(first av) (first bv)])) (recur (vec (rest av)) (vec (rest bv)) common (conj acc (seq-diff (first av) (first bv))))
                   (= 0 a-distance b-distance) (recur (vec (rest av)) (vec (rest bv)) (vec (rest common)) (conj acc (first common)))
                   (= a-distance b-distance) (recur (vec (rest av)) (vec (rest bv)) common (conj acc (if (= (first av) (first bv)) (first av) {:- (first av) :+ (first bv)}))))))]
    diff))

(defn seq-commit
  [a-seq diff]
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
        (every? map? [a pv mv]) (recur (rest as) (rest p) (rest m) (conj acc (map-commit a {:+ pv :- mv})))
        ;; if all seq? replace it with seq-commit
        (every? coll? [a pv mv]) (recur (rest as) (rest p) (rest m) (conj acc (seq-commit a {:+ pv :- mv})))
        ;; replace: a eq mv and pv !nil
        (and (= a mv) pv) (recur (rest as) (rest p) (rest m) (conj acc pv))
        ;; delete: a eq mv and pv nil
        (and (= a mv) (nil? pv)) (recur (rest as) (rest p) (rest m) acc)
        ;; insert: mv nil and pv !nil
        (and (nil? mv) pv) (recur as (rest p) (rest m) (conj acc pv))
        ;; keep: mv nil and pv nil
        (and (nil? mv) (nil? pv)) (recur (rest as) (rest p) (rest m) (conj acc a))))))
