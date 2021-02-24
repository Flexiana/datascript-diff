(ns seq-diff)

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
  (cond
    (< e (first acc)) (concat [e] acc)
    (> e (last acc)) (concat acc [e])
    :else acc))

(defn- ordered-parts
  [indexes]
  (->> (map (fn [idx]
              (reduce (fn [acc i]
                        (-> (for [a acc]
                              (glue-ordered a i))
                            (conj [i])))
                [[idx]]
                indexes)) indexes)
       (mapcat distinct)
       distinct))

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
                 (empty? common) (recur (vec (rest av)) (vec (rest bv)) [] (conj acc {:- (first av) :+ (first bv)}))
                 :else (let [a-distance (.indexOf av (first common))
                             b-distance (.indexOf bv (first common))]
                         (cond
                           (< a-distance b-distance) (recur av (vec (rest bv)) common (conj acc {:+ (first bv)}))
                           (> a-distance b-distance) (recur (vec (rest av)) bv common (conj acc {:- (first av)}))
                           (= 0 a-distance b-distance) (recur (vec (rest av)) (vec (rest bv)) (rest common) (conj acc (first common)))
                           (and (= a-distance b-distance) (map? (first av)) (map? (first bv)))
                           (recur (vec (rest av)) (vec (rest bv)) common (conj acc (map-diff (first av) (first bv))))
                           :else (recur (vec (rest av)) (vec (rest bv)) (rest common) (conj acc (if (= (first bv) (first av))
                                                                                                  (first av)
                                                                                                  {:+ (first bv) :- (first av)})))))))]
    {:+        (map :+ diff)
     :-        (map :- diff)
     :to-print (map #(get % :to-print %) diff)}))

(defn seq-commit
  [a-seq diff]
  (loop [as a-seq
         pv (:+ diff)
         mv (:- diff)
         acc []]
    (cond
      (empty? as) (reduce conj acc (remove nil? pv))
      (and (map? (first as)) (map? (first pv)) (map? (first mv))) (recur (rest as) (rest pv) (rest mv)
                                                                    (conj acc (map-commit (first as)
                                                                                          {:- (first mv)
                                                                                           :+ (first pv)})))
      (first pv) (recur as (rest pv) (rest mv) (conj acc (first pv)))
      (first mv) (recur (rest as) (rest pv) (rest mv) acc)
      :else (recur (rest as) (rest pv) (rest mv) (conj acc (first as))))))
