(ns ui
  (:require [diff :refer [prepare-print
                          make]]
            [nubank.workspaces.card-types.react :refer [react-card]]
            [nubank.workspaces.core :refer [mount defcard]]
            [reagent.core :as r]
            [clojure.tools.reader.edn :as edn]
            [clojure.string :as str]))

(defonce state (r/atom {}))

(defn update-diff
  [{:keys [map_1 map_2]}]
  (let [a_map (edn/read-string map_1)
        b_map (edn/read-string map_2)]
    (if (and (map? a_map) (map? b_map))
      (let [diffs (->>
                    (make a_map b_map)
                    (prepare-print a_map))]
        diffs)
      "")))

(defn diff
  [{:keys [map_1 map_2] :as state}]
  [:div
   [:textarea
    {:type          :textarea
     :rows          25
     :columns       80
     :auto-focus    true

     :on-change     #(swap! state assoc :map_1 (-> % .-target .-value))}]
   [:textarea
    {:type          :textarea
     :rows          25
     :columns       80
     :on-change     #(swap! state assoc :map_2 (-> % .-target .-value))}]
   [:textarea
    {:type     :textarea
     :rows     25
     :columns  80
     :value    (try
                 (str (update-diff @state))
                 (catch :default e ""))
     :readOnly true}]
   [:div
    (-> (try
          (reduce (fn colorizer
                    [acc [k v]]
                    (concat acc [[:span {:style {:color :black}} k]
                                 [:span {:style {:color :orange}} v]])) [] (update-diff @state))
          (catch :default e "")))]])

(defcard diff-card (react-card (r/as-element [diff state])))

(defonce run (mount))

#_(defn run
    []
    (dom/render
      [:div.panel-block.is-block
       [:div.container]
       [diff state]]

      (js/document.getElementById "app")))
