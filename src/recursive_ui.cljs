(ns recursive-ui
  (:require [recursive-diff :refer [prepare-print
                                    make]]
            [nubank.workspaces.card-types.react :refer [react-card]]
            [nubank.workspaces.core :refer [mount defcard]]
            [reagent.core :as r]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defonce state (r/atom {}))

(defn- diffs
  [{:keys [map_1 map_2]}]
  (let [a_map (edn/read-string map_1)
        b_map (edn/read-string map_2)
        diff (if (and (map? a_map) (map? b_map))
               (make a_map b_map)
               {})
        to-print (if (empty? diff)
                   ""
                   (prepare-print a_map diff))]
    [diff to-print]))

(defn- update-diff!
  [state]
  (let [[diff to-print] (try
                          (diffs @state)
                          (catch :default e [{} {}]))
        diff-str (if (empty? diff)
                   "No diff calculated"
                   (-> diff
                       str
                       (str/replace #",\s*" "\n")
                       (str/replace #"[\{|}]" "\n")
                       (str/replace #"\n+" "\n")
                       rest
                       str/join))]
    (swap! state assoc :to-print to-print)
    (swap! state assoc :diff-str diff-str)))

(defn- diff-div
  [color k v]
  [:div {:style {:background color
                 :width      :max-content}} (str k " " v)])

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
                 :overflow :auto
                 :resize :both
                 :min-width    "175px"
                 :border-style :solid
                 :border-width :thin
                 :border-color :gray
                 :height       "380px"}} text])

(defn- colorize
  ([diff]
   (colorize diff 2))
  ([diff depth]
   (let [c (colorize-core diff depth)]
     (if (not-empty diff) (conj (into (visual-div "{")
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
    (update-diff! state)
    [:div {:style {:margin :auto
                   :width :max-content}}
     [:table {:style {:margin :auto
                      :width :max-content}}
      [:thead [:tr
               [:th "Map A"] [:th "Map B"] [:th "Calculated diff"] [:th "Visual diff"]]]
      [:tbody [:tr
               [:td (text-area map_1 (partial on-change state :map_1))]
               [:td (text-area map_2 (partial on-change state :map_2))]
               [:td (text-area (:diff-str @state))]
               [:td {:style {:vertical-align :top}} colorized]]]]]))