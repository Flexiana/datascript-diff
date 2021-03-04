(ns differences
  (:require
   [diff :refer [all-paths map-diff]]
   [reagent.core :as r]
   [cljs.pprint :as pprint]
   [clojure.edn :as edn]))

(defn dissoc-in
  ([m ks]
   (if-let [[k & ks] (seq ks)]
     (if (seq ks)
       (let [v (dissoc-in (get m k) ks)]
         (if (empty? v)
           (dissoc m k)
           (assoc m k v)))
       (dissoc m k))
     m))
  ([m ks & kss]
   (if-let [[ks' & kss] (seq kss)]
     (recur (dissoc-in m ks) ks' kss)
     (dissoc-in m ks))))

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

(defn commit-diff->have! [*state {:keys [path expected actual mismatch] :as data}]
  (case mismatch
    :diff (swap! *state update :have-map
                 assoc-in path expected)
    :+    (swap! *state update :have-map
                 assoc-in path actual)
    :-    (swap! *state update :have-map dissoc-in path))
  (swap! *state #(assoc %
                        :txs
                        (-> %
                            :txs
                            (conj (assoc data :id (random-uuid))))
                        :have-map-input
                        (-> %
                            :have-map
                            str)
                        :want-map-input
                        (-> %
                            :want-map
                            str))))

(defn undo-commit! [*state txs id]
  (let [{:keys [path expected actual mismatch]} (first (filter #(= id (% :id)) txs))]
    (case mismatch
      :diff (swap! *state update :have-map
                   assoc-in path actual)
      :+ (swap! *state update :have-map
                dissoc-in path expected)
      :- (swap! *state update :have-map
                assoc-in path expected))
    (swap! *state (fn [st]
                    (assoc st
                           :txs
                           (->> st :txs (remove #(= id (% :id))))
                           :have-map-input
                           (-> st
                               :have-map
                               str)
                           :want-map-input
                           (-> st
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
               [:td [:label {:for "have-map"}
                     [:textarea {:style     {:font-size "18pt"}
                                 :type      :textarea
                                 :rows      25
                                 :columns   80
                                 :on-input #(let [input  (.. % -target -value)]
                                              (swap! *state assoc
                                                     :have-map-input input
                                                     :have-map (input->map input)))
                                 :value have-map-input}]]]
               [:td [:label {:for "want-map"}
                     [:textarea {:style     {:font-size "18pt"}
                                 :type      :textarea
                                 :rows      25
                                 :columns   80
                                 :on-input #(let [input  (.. % -target -value)]
                                              (swap! *state assoc
                                                     :want-map-input input
                                                     :want-map (input->map input)))
                                 :value want-map-input}]]]
               [:td [:label {:for "diffs"}
                     [map-diffs-ui *state @*state]]]
               [:td [:label {:for "txs"}
                     [txs-ui *state @*state]]]]]]]))
