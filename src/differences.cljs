(ns differences
  (:require
   [diff :refer [all-paths map-diff]]
   [reagent.core :as r]
   [cljs.pprint :as pprint]
   [clojure.edn :as edn]))

(defn map-diffs-from-editor
  [{:keys [have-map
           want-map]}]
  (map-diff have-map want-map))

(defn map-diffs-ui [state]
  [:table  {:style {:margin :auto
                    :border "1px solid black"
                    :border-collapse "collapse"
                    :text-align "center"}}
   [:thead [:tr {:style {:border "1px solid black"}}
            [:th {:style {:border "1px solid black"}}
             "Path"]
            [:th {:style {:border "1px solid black"}}
             "Expected"]
            [:th {:style {:border "1px solid black"}}
             "Actual"]]]
   [:tbody (map (fn [{:keys [path expected actual mismatch]}]
                  (when-not (empty? path)
                    [:tr {:key (apply str [path expected actual mismatch])
                          :style {:border "1px solid black"
                                  :background-color (case mismatch
                                                      :+ "rgba(51, 170, 51, .7)"
                                                      :- "rgba(170, 51, 51, .7)"
                                                      :diff "rgba(254, 241, 96, 0.7)")}}
                     [:td {:style {:border "1px solid black"}}
                      (str path)]
                     [:td {:style {:border "1px solid black"}}
                      (str expected)]
                     [:td {:style {:border "1px solid black"}}
                      (str actual)]
                     [:td {:style {:border "1px solid black"}}
                      (name mismatch)]]))
                (map-diffs-from-editor state))]])

(defonce *editor (r/atom {}))

(defn ui [state]
  [:div {:style {:margin :auto
                 :width  :max-content}}
   [:table {:style {:margin :auto
                    :width  :max-content}}
    [:thead [:tr
             [:th "Have Map"]
             [:th "Want Map"]
             [:th "Diffs"]]]
    [:tbody [:tr
             [:td [:label {:for "have-map"}
                   [:textarea {:style     {:font-size "18pt"}
                               :type      :textarea
                               :rows      25
                               :columns   80
                               :on-change #(swap! state assoc :have-map (try
                                                                          (edn/read-string
                                                                           (.. %
                                                                               -target
                                                                               -value))
                                                                          (catch js/Error e
                                                                            nil)))}]]]
             [:td [:label {:for "want-map"}
                   [:textarea {:style     {:font-size "18pt"}
                               :type      :textarea
                               :rows      25
                               :columns   80
                               :on-change #(swap! state assoc :want-map (try
                                                                          (edn/read-string
                                                                           (.. %
                                                                               -target
                                                                               -value))
                                                                          (catch js/Error e
                                                                            nil)))}]]]
             [:td [:label {:for "diffs"}
                   [map-diffs-ui @state]]]]]]])
