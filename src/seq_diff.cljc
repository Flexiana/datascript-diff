(ns seq-diff)

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
  (reduce
    (fn [acc i]
      (-> (for [a acc]
            (glue-ordered a i))
          (conj [i]))) [] indexes))

(defn common-ordered-part
  [x y]
  (let [y-in-x (reduce concat (for [e y] (get-indexes x e)))
        x-in-y (reduce concat (for [e x] (get-indexes y e)))
        idx-y-in-x (reduce longer (ordered-parts y-in-x))
        idx-x-in-y (reduce longer (ordered-parts x-in-y))
        common-part (if (< (count idx-x-in-y) (count idx-y-in-x))
                      (map (vec y) idx-x-in-y)
                      (map (vec x) idx-y-in-x))]
    (println y-in-x idx-y-in-x)
    (println x-in-y idx-x-in-y)
    common-part))

(common-ordered-part '(:a :c :3 :c :b) [0 :a :c :b :3 :c])
