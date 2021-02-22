(ns ui
  (:require [diff :refer [prepare-print
                          make]]
            [nubank.workspaces.card-types.react :refer [react-card]]
            [nubank.workspaces.core :refer [mount defcard]]
            [reagent.core :as r]
            [clojure.tools.reader.edn :as edn]))

(defonce state (r/atom {}))

(defn update-diff
  [{:keys [map_1 map_2]}]
  (let [a_map (edn/read-string map_1)
        b_map (edn/read-string map_2)]
    (when (and (map? a_map) (map? b_map))
      (->>
        (make a_map b_map)
        (prepare-print  a_map)
        str))))

(defn diff
  [{:keys [map_1 map_2] :as state}]
  [:div {:on-submit #()}
   [:form
    [:textarea
     {:type :textarea
      :rows 25
      :columns 80
      :auto-focus true
      :default-value map_1
      :on-change #(swap! state assoc :map_1 (-> % .-target .-value))}]
    [:textarea
     {:type :textarea
      :rows 25
      :columns 80
      :default-value map_2
      :on-change #(swap! state assoc :map_2 (-> % .-target .-value))}]
    [:textarea
     {:type     :textarea
      :rows     25
      :columns  80
      :value    (try
                  (update-diff @state)
                  (catch :default e ""))
      :readOnly true}]]])

(defcard valami (react-card (r/as-element [diff state])))

(defonce run (mount))

#_(defn run
    []
    (dom/render
      [:div.panel-block.is-block
       [:div.container]
       [diff state]]

      (js/document.getElementById "app")))
