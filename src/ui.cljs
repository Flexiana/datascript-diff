(ns ui
  (:require [diff :refer [prepare-print
                          make]]
            [nubank.workspaces.card-types.react :refer [react-card]]
            [nubank.workspaces.core :refer [mount defcard]]
            [reagent.core :as r]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as str]))

(defonce state (r/atom {}))

(defn- update-diff
  [{:keys [map_1 map_2]}]
  (let [a_map (edn/read-string map_1)
        b_map (edn/read-string map_2)
        diff (if (and (map? a_map) (map? b_map))
               (->>
                 (make a_map b_map))
               "")]
    [diff (prepare-print a_map diff)]))


(defn- colorize-core [diff depth]
  (reduce (fn [acc [k v]]
            (let [add (str (:+ v))
                  rem (str (:- v))]
              (conj acc (cond-> [:div {:style {:padding-left (str (* 0.2 depth) "em")}}]
                                (and (every? empty? [rem add]) (map? v)) (conj [:div (str k "{")])
                                (not-empty rem) (conj [:div {:style {:background :lightcoral}} (str k " " rem)])
                                (not-empty add) (conj [:div {:style {:background :lightgreen}} (str k " " add)])
                                (and (not (map? v)) (every? empty? [rem add])) (conj [:div {:style {:background :lightgray}} (str k " " v)])
                                (and (map? v) (every? empty? [rem add])) (conj (conj (colorize-core v (inc depth)) "}")))))) [:div] diff))

(defn- visual-div [text]
  [:div {:style {:width        :max-content
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

(defn diff
  [state]
  (let [{:keys [map_1 map_2]} @state
        [the-diff to-print] (try
                              (update-diff @state)
                              (catch :default e [{} {}]))
        colorized (colorize to-print)]
    [:div
     [:table
      [:tr [:th "Map A"] [:th "Map B"] [:th "Calculated diff"] [:th "Visual diff"]]
      [:tr [:td [:textarea
                 {:type          :textarea
                  :rows          25
                  :columns       80
                  :auto-focus    true
                  :default-value map_1
                  :on-change     #(swap! state assoc :map_1 (-> % .-target .-value))}]]
       [:td [:textarea
             {:type          :textarea
              :rows          25
              :columns       80
              :default-value map_2
              :on-change     #(swap! state assoc :map_2 (-> % .-target .-value))}]]
       [:td [:textarea
             {:type     :textarea
              :rows     25
              :columns  80
              :value    (if (empty? the-diff)
                          "No diff calculated"
                          (-> the-diff
                            str
                            (str/replace #",\s*" "\n")
                            (str/replace #"[\{|}]" "\n")
                            (str/replace #"\n+" "\n")
                            rest
                            str/join))
              :readOnly true}]]
       [:td {:style {:vertical-align :top}} colorized]]]]))



(defcard diff-card (react-card (r/as-element [diff state])))

(defonce run mount)

#_(defn run
    []
    (dom/render
      [:div.panel-block.is-block
       [:div.container]
       [diff state]]

      (js/document.getElementById "app")))
