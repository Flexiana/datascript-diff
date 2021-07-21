(ns differences
  (:require
    [clojure.edn :as edn]
    [diff :refer [map-diff
                  commit-diff
                  uncommit]]
    [reagent.core :as r]))


(defn input->map
  [input]
  (try
    (edn/read-string
      input)
    (catch js/Error e
      e)))


(defn map-diffs-from-editor
  [{:keys [have-map
           want-map] :as state}]
  (merge state
         (map-diff have-map want-map)))


(defn commit-diff!
  [*state diffs diff]
  (swap! *state
         (fn [st]
           (let [{:keys [have-map] :as updated-st}
                 (commit-diff st diffs diff)]
             (assoc updated-st
                    :have-map-input (str have-map))))))


(defn uncommit!
  [*state txs id]
  (swap! *state
         (fn [st]
           (let [updated-st  (update st :have-map (partial uncommit txs id))]
             (assoc updated-st :txs
                    (->> updated-st
                         :txs
                         (remove #(= id (% :id))))
                    :have-map-input
                    (-> updated-st
                        :have-map
                        str)
                    :want-map-input
                    (-> updated-st
                        :want-map
                        str))))))


(defn map-diffs-ui
  [*state {:keys [txs diffs] :as state}]
  [:table
   [:thead [:tr {:style {:border "1px solid black"}}
            [:th {:style {:border "1px solid black"}}
             "Path"]
            [:th {:style {:border "1px solid black"}}
             "Expected"]
            [:th {:style {:border "1px solid black"}}
             "Actual"]
            [:th {:style {:border "1px solid black"}}
             "Mismatch"]]]
   [:tbody (let [diffs (->> state
                            map-diffs-from-editor
                            :diffs)]
             (map
               (fn [{:keys [path expected actual mismatch] :as diff}]
                 (when-not (empty? path)
                   [:tr {:key   (apply str [path expected actual mismatch])
                         :style {:border           "1px solid black"
                                 :background-color (case mismatch
                                                     :+    "rgba(51, 170, 51, .7)"
                                                     :-    "rgba(170, 51, 51, .7)"
                                                     :diff "rgba(254, 241, 96, 0.7)")}}
                    [:td {:style {:border "1px solid black"}}
                     (str path)]
                    [:td {:style {:border "1px solid black"}}
                     (str expected)]
                    [:td {:style {:border "1px solid black"}}
                     (str actual)]
                    [:td {:style {:border "1px solid black"}}
                     (name mismatch)]
                    [:td
                     [:button {:on-click #(commit-diff! *state diffs diff)}
                      "Commit Diff"]]])) diffs))]])


(defn txs-ui
  [*state {:keys [txs]}]
  [:table {:style {:margin          :auto
                   :border          "1px solid black"
                   :border-collapse "collapse"
                   :text-align      "center"}}
   [:thead [:tr {:style {:border "1px solid black"}}
            [:th {:style {:border "1px solid black"}}
             "Id"]
            [:th {:style {:border "1px solid black"}}
             "Path"]
            [:th {:style {:border "1px solid black"}}
             "Expected"]
            [:th {:style {:border "1px solid black"}}
             "Actual"]
            [:th {:style {:border "1px solid black"}}
             "Mismatch"]]]
   [:tbody (map (fn [{:keys [id path expected actual mismatch]}]
                  [:tr {:key   (apply str [id path expected actual mismatch])
                        :style {:border "1px solid black"}}
                   [:td {:style {:border "1px solid black"}}
                    (str id)]
                   [:td {:style {:border "1px solid black"}}
                    (str path)]
                   [:td {:style {:border "1px solid black"}}
                    (str expected)]
                   [:td {:style {:border "1px solid black"}}
                    (str actual)]
                   [:td {:style {:border "1px solid black"}}
                    (name mismatch)]
                   [:td
                    [:button {:on-click (partial uncommit! *state txs id)}
                     "Undo Commit"]]])
                txs)]])


(defonce *editor (r/atom {:have-map-input ""
                          :want-map-input ""
                          :txs []}))


(defn ui
  [*state]
  (let [{:keys [have-map-input
                want-map-input]} @*state]
    [:div {:style {:margin :auto
                   :width  :max-content}}
     [:table {:style {:margin :auto
                      :width  :max-content}}
      [:thead [:tr
               [:th "Have Map"]
               [:th "Want Map"]
               [:th "Diffs"]
               [:th "Commited"]]]
      [:tbody [:tr
               [:td [:textarea {:style     {:font-size "18pt"}
                                :type      :textarea
                                :rows      25
                                :columns   80
                                :on-change #(let [input (.. % -target -value)]
                                              (swap! *state assoc
                                                     :have-map-input input
                                                     :have-map (input->map input)))
                                :value     have-map-input}]]
               [:td [:textarea {:style     {:font-size "18pt"}
                                :type      :textarea
                                :rows      25
                                :columns   80
                                :on-change #(let [input (.. % -target -value)]
                                              (swap! *state assoc
                                                     :want-map-input input
                                                     :want-map (input->map input)
                                                     :txs []))
                                :value     want-map-input}]]
               [:td {:style {:vertical-align "top"}}
                [map-diffs-ui *state @*state]]
               [:td {:style {:vertical-align "top"}}
                [txs-ui *state @*state]]]]]]))
