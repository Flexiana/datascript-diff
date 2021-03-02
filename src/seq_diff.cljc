(ns seq-diff)

(defn- map-diff
  [a b]
  ((resolve 'map-diff/map-diff) a b))

(defn- map-commit
  [a b]
  ((resolve 'map-diff/map-commit) a b))

(defn map-revert-diff
  [v]
  ((resolve 'map-diff/map-revert-diff) v))

(defn not-map-but-coll?
  [x]
  (and (not (map? x)) (coll? x)))

(defn- get-indexes
  [v e]
  (keep-indexed (fn [idx v] (when (= e v) idx)) (vec v)))

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
  (loop [av (vec a-seq)
         bv (vec b-seq)
         common (common-ordered-part a-seq b-seq)
         acc []]
    (let [a-distance (when common (.indexOf av (first common)))
          b-distance (when common (.indexOf bv (first common)))
          fav (first av)
          fbv (first bv)
          rvb (vec (rest bv))
          rav (vec (rest av))]
      (cond
        (every? empty? [av bv]) acc
        ;(empty? av) (concat acc (map (fn [x] {:+ x}) bv))
        ;(empty? bv) (concat acc (map (fn [x] {:- x}) av))
        (< a-distance b-distance) (recur av rvb common (conj acc {:+ (or fbv :nil)}))
        (> a-distance b-distance) (recur rav bv common (conj acc {:- (or fav :nil)}))
        (and (= a-distance b-distance) (every? map? [fav fbv])) (recur rav rvb common (conj acc (map-diff fav fbv)))
        (and (= a-distance b-distance) (every? not-map-but-coll? [fav fbv])) (recur rav rvb common (conj acc (seq-diff fav fbv)))
        (= 0 a-distance b-distance) (recur rav rvb (vec (rest common)) (conj acc (if fav nil :nil)))
        (= a-distance b-distance) (recur rav rvb common (conj acc (if (= fav fbv) fav {:- fav :+ fbv})))))))

(defn extend-seq
  [s d]
  (loop [s s
         d d
         acc []]
    (let [fs (first s)
          fd (first d)
          dp (:+ fd)
          dm (:- fd)]
      (cond
        (empty? s) (concat acc (repeat (count d) nil))
        (and dp dm) (recur (rest s) (rest d) (conj acc fs))
        dp (recur s (rest d) (conj acc nil))
        :else (recur (rest s) (rest d) (conj acc fs))))))

(defn seq-commit
  [a-seq diff]
  (let [extended (extend-seq a-seq diff)
        merged (map (fn [a d] [a d]) extended diff)]
    (reduce (fn [acc [orig change]]
              (let [mv (:- change)
                    pv (:+ change)]
                (cond
                  pv (conj acc pv)
                  mv acc
                  (every? map? [orig change]) (conj acc (map-commit orig change))
                  (every? not-map-but-coll? [orig change]) (conj acc (seq-commit orig change))
                  (every? nil? [pv mv]) (conj acc orig))))
      [] merged)))

(defn seq-revert-diff
  [diff]
  (map (fn [v]
         (let [pv (:+ v)
               mv (:- v)]
           (cond
             (or pv mv) (cond-> {}
                          pv (assoc :- pv)
                          mv (assoc :+ mv))
             (map? v) (map-revert-diff v)
             (coll? v) (seq-revert-diff v)
             :else v)))
       diff))

(defn seq-revert
  [b-seq diff]
  (->> (seq-revert-diff diff)
       (seq-commit b-seq)))
