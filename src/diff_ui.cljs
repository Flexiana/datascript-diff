(ns diff-ui
  (:require [map-diff :refer [map-diff
                              map-commit]]
            [seq-diff :refer [extend-seq
                              seq-diff
                              seq-commit]]
            [reagent.core :as r]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defonce state (r/atom {}))

(defn commit
  [a d]
  (if (map? a)
    (map-commit a d)
    (seq-commit a d)))

(defn but
  [x y]
  (and (not x) y))

(defn make-diff
  [a b]
  (cond
    (every? map? [a b]) (map-diff a b)
    (but
      (some map? [a b])
      (every? coll? [a b])) (seq-diff a b)
    :else {}))

(defn- diffs
  [{:keys [a-input b-input]}]
  (let [a (edn/read-string a-input)
        b (edn/read-string b-input)

        difference (make-diff a b)]
    (if (empty? difference)
      [{} {} "" {}]
      [a difference (commit a difference)])))

(defn- update-diff!
  [state]
  (let [[a-value diff commit] (try
                                (swap! state dissoc :error)
                                (diffs @state)
                                (catch :default e
                                  (swap! state assoc :error e)
                                  [{} {} {}]))
        diff-str (if (empty? diff)
                   "No diff calculated"
                   (-> diff
                       str
                       (str/replace #",\s*" "\n")
                       (str/replace #"[\{|}]" "\n")
                       (str/replace #"\n+" "\n")
                       rest
                       str/join))
        commit (if (empty? commit)
                 "No diff calculated"
                 (str commit))]
    (swap! state assoc
      :a-value a-value
      :diff diff
      :commit commit
      :diff-str diff-str)))

(defn- diff-div
  [color k v]
  [:div (str k " ") [:a {:style {:background color
                                 :width      :max-content}} v]])

(defn seq-merge
  [a v]
  (let [empty-diff (map (fn [p m]
                          (cond-> nil
                            p (assoc :+ p)
                            m (assoc :- m))) (:+ v) (:- v))]
    (loop [e empty-diff
           original a
           acc []]
      (cond (empty? e) (concat acc original)
            (nil? (first e)) (recur (rest e) (rest original) (conj acc (first original)))
            (every? map? [(first original) (:+ (first e))]) (recur (rest e) (rest original) (conj acc (first e)))
            (every? coll? [(first original) (:+ (first e))]) (recur (rest e) (rest original) (conj acc (seq-merge (first original) (first e))))
            (:- (first e)) (recur (rest e) (rest original) (conj acc (first e)))
            :else (recur (rest e) original (conj acc (first e)))))))

(defn table-row
  []
  ^{:key (gensym)}
  [:tr
   {:style {:border-bottom   "5px"
            :border-top      "5px"
            :border-style    :solid
            :text-align      :center
            :border-collapse :collapse}}])

(defn table
  [body]
  [:div [:div {:style {:overflow     :auto
                       :border-style :solid
                       :border-width :thin
                       :border-color :gray}}
         [:table {:style {:cellpadding "0px"}}
          [:tbody body]]]])

(defn td-border
  ([]
   ^{:key (gensym)}
   [:td {:style {:border-bottom :solid
                 :border-width  :thin}}])
  ([color content]
   ^{:key (gensym)}
   [:td {:style {:border-bottom :solid
                 :border-width  :thin
                 :background    color}} content]))

(defn td
  ([]
   ^{:key (gensym)}
   [:td])
  ([content]
   ^{:key (gensym)}
   [:td content])
  ([color content]
   ^{:key (gensym)}
   [:td {:style {:background color}} content]))

(defn merge-map
  [a-map diff]
  (->> (reduce (fn [acc [k v]]
                 (if (some #(= k (first %)) (keys acc))
                   acc
                   (assoc acc (vector k) v))) diff a-map)
       (group-by ffirst)
       (into {})))

(merge-map {:a 2 :b 2} {[:c :d] 2 [:c :a] 4})

(defn logit
  [param1 param2]
  (println param1 param2)
  param2)

(defn- colorize-core
  [a-value diff]
  (letfn [(color-seq
            [s]
            (let [extended (extend-seq a-value diff)
                  merged (map (fn [a d] [a d]) extended diff)]
              (println "merged " merged)
              (table
                ^{:key (gensym)}
                [:tr (td "(")
                 (into (table-row)
                       (for [[origin change] merged]
                         (let [pv (:+ change)
                               mv (:- change)]
                           (cond
                             (nil? origin) (td)
                             mv (td :lightcoral (str mv))
                             (every? coll? [origin change]) (td (colorize-core origin change))
                             origin (td :lightgrey (str origin))))))
                 (into (table-row)
                       (for [c s]
                         (cond
                           (:+ c) (td :lightgreen (str (:+ c)))
                           :else (td))))
                 (td ")")])))
          (color-map2
            [a-map diff]
            (let [preprocessed (merge-map a-map diff)]
              (table
                ^{:key (gensym)}
                [:tr
                 (td "{")
                 (mapcat distinct
                         (for [[k w] preprocessed]
                           (mapcat distinct
                                   (for [[ks v] w]
                                     (let [pv (:+ v)
                                           mv (:- v)
                                           origin (get a-map k)]
                                       [(into (table-row)
                                              (cond
                                                (next ks) [(td-border :white (str k)) (colorize-core origin {(rest ks) v})]
                                                (and pv mv) [(td :white (str k)) (td :lightcoral (str mv))]
                                                mv [(td-border :lightcoral (str k)) (td-border :lightcoral (str mv))]
                                                pv [(td) (td)]
                                                (map? v) [(td-border :white (str k)) (colorize-core origin {(rest ks) v})]
                                                (coll? v) [(td-border :white (str k)) (colorize-core origin v)]
                                                :else [(td (str k)) (td (str v))]))
                                        (into (table-row)
                                              (cond
                                                (and mv pv) [(td-border) (td-border :lightgreen (str pv))]
                                                (and (not (coll? v)) pv) [(td (str k)) (td)]
                                                pv [(td-border :lightgreen (str k)) (td-border :lightgreen (str pv))]
                                                (coll? v) [(td) (td)]
                                                :else [(td) (td)]))])))))
                 (td "}")])))]
    (if
      (map? diff)
      (color-map2 a-value diff)
      (color-seq diff))))

(defn- visual-div
  [text]
  [:div {:style {:width        :max-content
                 :overflow     :auto
                 :resize       :both
                 :min-width    "175px"
                 :border-style :solid
                 :border-width :thin
                 :border-color :gray
                 :height       "380px"}} text])

(defn- colorize
  [a-value diff]
  (let [c (colorize-core a-value diff)]
    (if (not-empty diff)
      (into (visual-div "")
            (rest c))
      (visual-div "No diff calculated"))))

(defn- text-area
  ([value]
   [:textarea
    {:type     :textarea
     :rows     25
     :columns  80
     :value    value
     :readOnly true}])
  ([value on-change]
   [:textarea
    {:type      :textarea
     :rows      25
     :columns   80
     :value     value
     :on-change on-change}]))

(defn- on-change
  [state k event]
  (swap! state assoc k (-> event .-target .-value))
  (update-diff! state))

(defn diff
  [state]
  (let [{:keys [a-input b-input a-value diff]} @state
        colorized (colorize a-value diff)]
    [:div {:style {:margin :auto
                   :width  :max-content}}
     [:table#main {:style {:margin :auto
                           :width  :max-content}}
      [:thead [:tr
               [:th "Map A"]
               [:th "Map B"]
               [:th "Calculated diff"]
               [:th "Visual diff"]
               [:th "Commit diff on Map A"]]]
      [:tbody [:tr
               [:td (text-area a-input (partial on-change state :a-input))]
               [:td (text-area b-input (partial on-change state :b-input))]
               [:td (text-area (get @state :diff-str "No diff calculated"))]
               [:td {:style {:vertical-align :top}} colorized]
               [:td (text-area (get @state :commit "No diff calculated"))]]]]
     [:p (str (get @state :error " "))]
     [:p (str (get @state :diff " "))]]))
