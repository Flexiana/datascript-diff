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
                                                                            e)))}]]]
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
                                                                            e)))}]]]
             [:td [:label {:for "diffs"}
                   [:ul
                    (keep (fn [{:keys [path expected actual mismatch]}]
                            [:<>
                             [:li
                              (reduce #(str %1 %2 " ") ""
                                      [path expected actual mismatch])]])
                          (map-diffs-from-editor @*editor))]]]]]]])
