(ns differences
  (:require
   [diff :refer [all-paths map-diff dissoc-in remove-idxs remove-range-to-last-idx]]
   [reagent.core :as r]
   [clojure.edn :as edn]))

(defn input->map [input]
  (try
    (edn/read-string
     input)
    (catch js/Error e
      e)))

(defn map-diffs-from-editor
  [{:keys [have-map
           want-map]}]
  (map-diff have-map want-map))

(defn commit-diff->have [{:keys [path expected actual mismatch] :as data} have-map-st]
  (case mismatch
    :diff (assoc-in have-map-st path expected)
    :+ (assoc-in have-map-st path actual)
    :- (if (vector? have-map-st)
         (remove-range-to-last-idx path have-map-st)
         (dissoc-in have-map-st path))))

(defn commit-diff->have! [*state data]
  (swap! *state
         (fn [st]
           (let [updated-st (update st :have-map (partial commit-diff->have data))]
             (assoc updated-st
                    :txs
                    (-> updated-st
                        :txs
                        (conj (assoc data :id (random-uuid))))
                    :have-map-input
                    (-> updated-st
                        :have-map
                        str)
                    :want-map-input
                    (-> updated-st
                        :want-map
                        str))))))

(defn undo-have-map-commit [txs id have-map-st]
  (let [{:keys [path expected actual mismatch]} (first (filter #(= id (% :id)) txs))]
    (case mismatch
      :diff (assoc-in have-map-st path actual)
      :+    (dissoc-in have-map-st path expected)
      :-    (assoc-in have-map-st path expected))))

(defn undo-commit! [*state txs id]
  (swap! *state
         (fn [st]
           (let [updated-st  (update st :have-map (partial undo-have-map-commit txs id))]
             (assoc updated-st :txs
                    (->> updated-st :txs (remove #(= id (% :id))))
                    :have-map-input
                    (-> updated-st
                        :have-map
                        str)
                    :want-map-input
                    (-> updated-st
                        :want-map
                        str))))))

(defn map-diffs-ui [*state state]
  [:table  {:style {:margin          :auto
                    :border          "1px solid black"
                    :border-collapse "collapse"
                    :text-align      "center"}}
   [:thead [:tr {:style {:border "1px solid black"}}
            [:th {:style {:border "1px solid black"}}
             "Path"]
            [:th {:style {:border "1px solid black"}}
             "Expected"]
            [:th {:style {:border "1px solid black"}}
             "Actual"]
            [:th {:style {:border "1px solid black"}}
             "Mismatch"]]]
   [:tbody (map (fn [{:keys [path expected actual mismatch] :as d}]
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
                     [:button {:on-click #(do (js/console.log [d *state])
                                              (commit-diff->have! *state d)
                                              (js/console.log [d *state]))}
                      "Commit Diff"]]))
                (map-diffs-from-editor state))]])

(defn txs-ui [*state {:keys [txs]}]
  [:div
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
                         :style {:border           "1px solid black"}}
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
                    [:button {:on-click (partial undo-commit! *state txs id)}
                     "Undo Commit"]])
                 txs)]]])


(defonce *editor (r/atom {:have-map-input ""
                          :want-map-input ""
                          :txs []}))

(defn ui [*state]
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
                                                     :want-map (input->map input)))
                                :value     want-map-input}]]
               [:td {:style {:vertical-align "top"}}
                [map-diffs-ui *state @*state]]
               [:td {:style {:vertical-align "top"}}
                [txs-ui *state @*state]]]]]]))
