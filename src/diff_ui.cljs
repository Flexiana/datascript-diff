(ns diff-ui
  (:require [map-diff :refer [prepare-print
                              map-diff
                              map-commit]]
            [seq-diff :refer [seq-diff
                              seq-commit]]
            [reagent.core :as r]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defonce state (r/atom {}))

(defn logit
  ([x]
   (println x)
   x)
  ([m x]
   (println m x)
   x))

(defn commit
  [a d]
  (if (map? a)
    (map-commit a d)
    (seq-commit a d)))

(defn make-diff
  [a b]
  (cond
    (every? map? [a b]) (map-diff a b)
    (every? coll? [a b]) (seq-diff a b)
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
                                (catch :default e (do (swap! state assoc :error e) [{} {} "" {}])))
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

(defn s-v?
  [x]
  (or (seq? x) (vector? x)))

(defn no-op
  [param1]
  (println param1)
  param1)

(defn but
  [x y]
  (and (not x) y))

(defn seq-merge
  [a v]
  (let [empty-diff (map (fn [p m]
                          (cond-> nil
                            p (assoc :+ p)
                            m (assoc :- m))) (:+ v) (:- v))]
    (logit "e-d" empty-diff)
    (logit "a" a)
    (loop [e empty-diff
           original a
           acc []]
      (cond (empty? e) acc
            (nil? (first e)) (recur (rest e) (rest original) (conj acc (first original)))
            (:- (first e)) (recur (rest e) (rest original) (conj acc (first e)))
            :else (recur (rest e) original (conj acc (first e)))))))

(defn- colorize-core
  [a-value diff]
  (letfn [(color-seq
            [s]
            (logit s)
            [:div [:div {:style {:overflow     :auto
                                 :border-style :solid
                                 :border-width :thin
                                 :border-color :gray}}
                   [:table {:style {:border-spacing 0
                                    :cellpadding    "0px"}}
                    [:tbody
                     (into [:tr {:style {:border-bottom   "5px"
                                         :border-top      "5px"
                                         :border-style    :solid
                                         :border-collapse :collapse}} "("]
                           (concat (for [c s]
                                     [:td
                                      (cond
                                        (and (coll? c) (nil? (:- c)) (nil? (:+ c))) (colorize-core (a-value (.indexOf (vec s) c)) {:to-print c})
                                        (:- c) [:a {:style {:background     :lightcoral
                                                            :border-spacing "0px"}} (str (:- c))]
                                        (not (:+ c)) [:a {:style {:background     :lightgrey
                                                                  :border-spacing "0px"}} (str c)])]) [")"]))
                     (into [:tr]
                           (for [c s]
                             [:td (cond
                                    (:+ c) [:a {:style {:background     :lightgreen
                                                        :border-spacing "0px"}} (str (:+ c))]
                                    (not (:- c)) "")]))]]]])
          (color-map
            [acc [k v]]
            (let [pv (:+ v)
                  add (str pv)
                  mv (:- v)
                  rem (str mv)]
              (logit "a-value" a-value)
              (logit "c-map" [k ";" v])
              (conj acc (cond-> [:div]
                          (every? coll? [v mv pv]) (conj (str "" k) (colorize-core (get a-value k) {:to-print (logit "merged" (seq-merge (get a-value k) v))}))
                          (but (every? coll? [mv pv]) mv) (conj (diff-div :lightcoral k rem))
                          (but (every? coll? [mv pv]) pv) (conj (diff-div :lightgreen k add))
                          (and (not (map? v)) (every? nil? [mv pv])) (conj (diff-div :lightgray k (str v)))
                          (logit "map-in-map" (and (map? v) (every? nil? [mv pv]))) (conj (str k) (colorize-core (get a-value k) {:to-print v}))))))
          (color-map2
            [a m]
            [:div [:div {:style {:overflow     :auto
                                 :border-style :solid
                                 :border-width :thin
                                 :border-color :gray}}
                   [:table {:style {:border-spacing 0
                                    :cellpadding    "0px"}}
                    [:tbody
                     [:tr [:td "{"]
                      (mapcat distinct
                              (for [[k v] m]
                                [(into [:tr {:style {:border-bottom   "5px"
                                                     :border-top      "5px"
                                                     :border-style    :solid
                                                     :border-collapse :collapse}}]
                                       (cond
                                         (and (map? v) (every? coll? [(:- v) (:+ v)])) [[:td (str k)] [:td (logit "inner table" (colorize-core (logit "inner value" (get a-value k)) (logit "diff" {:to-print (seq-merge (get a-value k) v)})))]]
                                         (and (:- v) (:+ v)) [[:td (str k)]
                                                              [:td {:style {:background :lightcoral}} (str (:- v))]]
                                         (:- v) [[:td {:style {:background    :lightcoral
                                                               :border-bottom :solid
                                                               :border-width  :thin}} (str k)]
                                                 [:td {:style {:background    :lightcoral
                                                               :border-bottom :solid
                                                               :border-width  :thin}} (str (:- v))]]

                                         (and (map? v) (every? nil? [(:- v) (:+ v)])) [[:td (str k)] [:td (colorize-core (get a-value k) {:to-print v})]]
                                         (every? nil? [(:- v) (:+ v)]) [[:td {:style {:background    :lightgrey
                                                                                      :border-bottom :solid
                                                                                      :border-width  :thin}} (str k)]
                                                                        [:td {:style {:background    :lightgrey
                                                                                      :border-bottom :solid
                                                                                      :border-width  :thin}} (str v)]]))
                                 (into [:tr]
                                       (cond
                                         (and (map? v) (every? coll? [(:- v) (:+ v)])) [[:td]]
                                         (and (nil? (:- v)) (:+ v)) [[:td {:style {:background :lightgreen}} (str k)]
                                                                     [:td {:style {:background :lightgreen}} (str (:+ v))]]
                                         (:+ v) [[:td {:style {:border-bottom :solid
                                                               :border-width  :thin}}]
                                                 [:td {:style {:border-bottom :solid
                                                               :border-width  :thin
                                                               :background    :lightgreen}} (str (:+ v))]]))]))
                      [:td "}"]]]]]])]

    (if
      (map? (:to-print diff))
      (color-map2 a-value (:to-print diff))
      ;(reduce color-map [:div] (:to-print diff))
      (color-seq (:to-print diff)))))

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
  [{:keys [a-value diff]}]
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
  (let [{:keys [a-input b-input]} @state
        colorized (colorize @state)]
    [:div {:style {:margin :auto
                   :width  :max-content}}
     [:table {:style {:margin :auto
                      :width  :max-content}}
      [:thead [:tr
               [:th "Map A"] [:th "Map B"] [:th "Calculated diff"] [:th "Visual diff"] [:th "Commit diff on Map A"]]]
      [:tbody [:tr
               [:td (text-area a-input (partial on-change state :a-input))]
               [:td (text-area b-input (partial on-change state :b-input))]
               [:td (text-area (get @state :diff-str "No diff calculated"))]
               [:td {:style {:vertical-align :top}} colorized]
               [:td (text-area (get @state :commit "No diff calculated"))]]]]
     [:p (str (get-in @state [:diff :to-print]))]]))
