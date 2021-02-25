(ns diff-ui
  (:require [map-diff :refer [prepare-print
                              map-diff
                              map-commit]]
            [seq-diff]
            [reagent.core :as r]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defonce state (r/atom {}))

(defn- diffs
  [{:keys [map_1 map_2]}]
  (let [a_map (edn/read-string map_1)
        b_map (edn/read-string map_2)
        diff (if (and (map? a_map) (map? b_map))
               (map-diff a_map b_map)
               {})
        to-print (if (empty? diff)
                   ""
                   (prepare-print a_map diff))
        comm (map-commit a_map diff)]
    [diff to-print comm]))

(defn- update-diff!
  [state]
  (let [[diff to-print commit-on] (try
                                    (diffs @state)
                                    (catch :default e [{} {} {}]))
        diff-str (if (empty? diff)
                   "No diff calculated"
                   (-> diff
                       str
                       (str/replace #",\s*" "\n")
                       (str/replace #"[\{|}]" "\n")
                       (str/replace #"\n+" "\n")
                       rest
                       str/join))
        applied-on (if (empty? commit-on)
                     "No diff calculated"
                     (str commit-on))]
    (swap! state assoc
      :to-print to-print
      :diff diff
      :commit applied-on
      :diff-str diff-str)))

(defn- diff-div
  [color k v]
  [:div {:style {:background color
                 :width      :max-content}} (str k " " v)])

(defn s-v?
  [x]
  (or (seq? x) (vector? x)))

(defn no-op
  [param1]
  (println param1)
  param1)

(defn- colorize-core
  [diff depth]
  (reduce (fn [acc [k v]]
            (let [add (str (:+ v))
                  rem (str (:- v))]
              (conj acc (cond-> [:div {:style {:padding-left (str (* 0.4 depth) "em")}}]
                          (and (every? empty? [rem add]) (map? v)) (conj [:div (str k " {")])
                          (not-empty rem) (conj (diff-div :lightcoral k rem))
                          (not-empty add) (conj (diff-div :lightgreen k add))
                          (and (not (map? v)) (every? empty? [rem add])) (conj (diff-div :lightgray k v))
                          (and (map? v) (every? empty? [rem add])) (conj (conj (colorize-core v (inc depth)) "}"))))))
    [:div]
    diff))

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
  ([diff]
   (colorize diff 1))
  ([diff depth]
   (let [c (colorize-core diff depth)]
     (if (not-empty diff)
       (conj (into (visual-div "{")
                   (rest c))
         "}")
       (visual-div "No diff calculated")))))

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
  (let [{:keys [map_1 map_2 to-print]} @state
        colorized (colorize to-print)]
    [:div {:style {:margin :auto
                   :width  :max-content}}
     [:table {:style {:margin :auto
                      :width  :max-content}}
      [:thead [:tr
               [:th "Map A"] [:th "Map B"] [:th "Calculated diff"] [:th "Visual diff"] [:th "Commit diff on Map A"]]]
      [:tbody [:tr
               [:td (text-area map_1 (partial on-change state :map_1))]
               [:td (text-area map_2 (partial on-change state :map_2))]
               [:td (text-area (get @state :diff-str "No diff calculated"))]
               [:td {:style {:vertical-align :top}} colorized]
               [:td (text-area (get @state :commit "No diff calculated"))]]]]
     [:p state]]))
