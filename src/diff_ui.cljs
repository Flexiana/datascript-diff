(ns diff-ui
  (:require [map-diff :refer [prepare-print
                              map-diff
                              map-commit]]
            [seq-diff :refer [seq-diff
                              seq-commit]]
            [reagent.core :as r]
            [clojure.string :as str]
            [clojure.edn :as edn]))

(defonce state (r/atom {}))

(defn logit
  ([x]
   (println x)
   x)
  ([m x]
   (println m x)
   x))

(defn commit
  [a d]
  (if (map? a)
    (map-commit a d)
    (seq-commit a d)))

(defn make-diff
  [a b]
  (cond
    (every? map? [a b]) (map-diff a b)
    (every? coll? [a b]) (logit (seq-diff a b))
    :else {}))

(defn- diffs
  [{:keys [map_1 map_2]}]
  (let [a (edn/read-string map_1)
        b (edn/read-string map_2)

        difference (make-diff a b)]
    (if (empty? difference)
      [{} "" {}]
      [difference (:to-print difference) (commit a difference)])))

(defn- update-diff!
  [state]
  (let [[diff to-print commit] (try
                                 (swap! state dissoc :error)
                                 (diffs @state)
                                 (catch :default e (do (swap! state assoc :error e) [{} "" {}])))
        diff-str (if (empty? diff)
                   "No diff calculated"
                   (-> diff
                       str
                       (str/replace #",\s*" "\n")
                       (str/replace #"[\{|}]" "\n")
                       (str/replace #"\n+" "\n")
                       rest
                       str/join))
        commit (if (empty? commit)
                 "No diff calculated"
                 (str commit))]
    (swap! state assoc
      :to-print to-print
      :diff diff
      :commit commit
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

(defn color-seq
  [s]
  (logit s)
  [:div [:table
         [:tbody
          (into [:tr]
                (for [c s]
                  [:td  (get c :- (str c))]))
          (into [:tr]
                (for [c s]
                  [:td  (:+ c)]))
          (into [:tr]
                (for [c s]
                  [:td  (:- c)]))]]])

(defn- colorize-core
  [diff depth]
  (letfn [(for-map
            [acc [k v]]
            (let [add (str (:+ v))
                  rem (str (:- v))]
              (conj acc (cond-> [:div {:style {:padding-left (str (* 0.4 depth) "em")}}]
                          (and (every? empty? [rem add]) (map? v)) (conj [:div (str k " {")])
                          (not-empty rem) (conj (diff-div :lightcoral k rem))
                          (not-empty add) (conj (diff-div :lightgreen k add))
                          (and (not (map? v)) (every? empty? [rem add])) (conj (diff-div :lightgray k v))
                          (and (map? v) (every? empty? [rem add])) (conj (conj (colorize-core v (inc depth)) "}"))))))]
    (if (map? diff)
      (reduce for-map [:div] diff)
      (color-seq diff))))

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
  [diff]
  (let [c (colorize-core diff 1)]
    (if (not-empty diff)
      (conj (into (visual-div "{")
                  (rest c))
        "}")
      (visual-div "No diff calculated"))))

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
