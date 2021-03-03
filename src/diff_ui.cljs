(ns diff-ui
  (:require [map-diff :refer [map-diff
                              map-revert
                              map-commit]]
            [seq-diff :refer [extend-seq
                              seq-diff
                              seq-revert
                              seq-commit]]
            [reagent.core :as r]
            [clojure.edn :as edn]))

(defonce state (r/atom {}))

(defn- not-map-but-coll?
  [x]
  (and (not (map? x)) (coll? x)))

(defn- commit
  [a d]
  (if (map? a)
    (map-commit a d)
    (seq-commit a d)))

(defn- revert
  [a d]
  (if (map? a)
    (map-revert a d)
    (seq-revert a d)))

(defn- diff
  [a b]
  (cond
    (every? map? [a b]) (map-diff a b)
    (every? not-map-but-coll? [a b]) (seq-diff a b)
    :else nil))

(defn- process-inputs
  [{:keys [a-input b-input]}]
  (let [a (edn/read-string a-input)
        b (edn/read-string b-input)
        difference (diff a b)
        commit (when difference (commit a difference))
        revert (when difference (revert b difference))]
    (if (empty? difference)
      []
      [a difference commit revert])))

(defn- update-diff!
  [state]
  (let [[a-value
         diff
         commit
         revert] (try
                   (swap! state dissoc :error)
                   (process-inputs @state)
                   (catch :default e
                     (swap! state assoc :error (.-message e))
                     []))]
    (swap! state assoc
      :a-value a-value
      :diff diff
      :commit commit
      :revert revert)))

(defn- diff-div
  [color k v]
  [:div (str k " ") [:a {:style {:background color
                                 :width      :max-content}} v]])

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

(defn td-underline
  ([]
   (td-underline :white ""))
  ([color content]
   ^{:key (gensym)}
   [:td {:style {:border-bottom :solid
                 :border-width  :thin
                 :background    color}} content]))

(defn td
  ([]
   (td :white ""))
  ([content]
   (td :white content))
  ([color content]
   ^{:key (gensym)}
   [:td {:style {:background color}} content]))

(defn- merge-map
  [a-map diff]
  (reduce (fn [acc [ks v]]
            (assoc-in acc (if (coll? ks) ks (vector ks)) v)) a-map diff))

(defn- colorize-core
  [a-value diff]
  (letfn [(color-seq
            [s]
            (let [extended (extend-seq a-value diff)
                  merged (map (fn [a d] [a d]) extended diff)]
              (table
                ^{:key (gensym)}
                (into [:tr]
                      [(td "(")
                       (into (table-row)
                             (for [[origin change] merged]
                               (cond
                                 (every? nil? [origin change]) (td)
                                 (= :nil change) (td :lightgrey "nil")
                                 (:- change) (td :lightcoral (str (:- change)))
                                 (every? coll? [origin change]) (colorize-core origin change)
                                 origin (td :lightgrey (str origin))
                                 :else (td))))
                       (into (table-row)
                             (for [c s]
                               (cond
                                 (= :nil (:+ c)) (td :lightgreen "nil")
                                 (:+ c) (td :lightgreen (str (:+ c)))
                                 :else (td))))
                       (td ")")]))))
          (color-map2
            [a-map diff]
            (let [preprocessed (merge-map a-map diff)]
              (table
                ^{:key (gensym)}
                (into [:tr]
                      [(td "{")
                       (mapcat distinct
                               (for [[k v] preprocessed]
                                 (let [pv (:+ v)
                                       mv (:- v)
                                       origin (get a-map k)]
                                   [(into (table-row)
                                          (cond
                                            (and pv mv) [(td :white (str k)) (td :lightcoral (str mv))]
                                            mv [(td-underline :lightcoral (str k)) (td-underline :lightcoral (str mv))]
                                            pv [(td) (td)]
                                            (map? v) [(td-underline :white (str k)) (colorize-core origin v)]
                                            (not-map-but-coll? v) [(td (str k)) (colorize-core origin v)]
                                            :else [(td-underline :lightgrey (str k)) (td-underline :lightgrey (str v))]))
                                    (into (table-row)
                                          (cond
                                            (and mv pv) [(td-underline) (td-underline :lightgreen (str pv))]
                                            pv [(td-underline :lightgreen (str k)) (td-underline :lightgreen (str pv))]
                                            :else [(td) (td)]))])))
                       (td "}")]))))]
    (cond
      (map? diff) (color-map2 a-value diff)
      (not-map-but-coll? diff) (color-seq diff))))

(defonce width (r/atom 175))

(defn- copy-master-width!
  []
  (let [w (try (-> js/document (.getElementById "master") .-offsetWidth)
               (catch :default e @width))]
    (str (reset! width w) "px")))

(defn- visual-div
  [text id]
  [:div
   {:id    id
    :style {:width        (if (= id "master")
                            :max-content
                            (copy-master-width!))
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
      (into (visual-div "" "master")
            (rest c))
      (visual-div "No diff calculated" "master"))))

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

(defn diff-ui
  [state]
  (let [{:keys [a-input b-input a-value diff]} @state
        colorized (colorize a-value diff)]
    [:div {:style {:margin :auto
                   :width  :max-content}}
     [:table {:style {:margin :auto
                      :width  :max-content}}
      [:thead
       [:tr
        [:th "Input A"]
        [:th "Input B"]
        [:th "Visual diff"]]]
      [:tbody
       [:tr
        [:td (text-area a-input (partial on-change state :a-input))]
        [:td (text-area b-input (partial on-change state :b-input))]
        [:td {:style {:vertical-align :top}} colorized]]]]
     [:table {:style {:margin :auto
                      :width  :max-content}}
      [:thead
       [:tr
        [:th "Revert diff on Input B"]
        [:th "Commit diff on Input A"]
        [:th "Calculated diff"]]]
      [:tbody
       [:tr
        [:td (text-area (str (get @state :revert "No diff calculated")))]
        [:td (text-area (str (get @state :commit "No diff calculated")))]
        [:td (visual-div (if diff (str diff) "No diff calculated") "slave")]]]]
     [:p (str (get @state :error " "))]]))
