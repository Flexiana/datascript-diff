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
    (every? coll? [a b]) (seq-diff a b)
    :else {}))

(defn- diffs
  [{:keys [a-input b-input]}]
  (let [a (edn/read-string a-input)
        b (edn/read-string b-input)

        difference (make-diff a b)]
    (if (empty? difference)
      [{} {} "" {}]
      [a difference (commit a difference)])))

(defn- update-diff!
  [state]
  (let [[a-value diff commit] (try
                                (swap! state dissoc :error)
                                (diffs @state)
                                (catch :default e (do (swap! state assoc :error e) [{} {} "" {}])))
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
      :a-value a-value
      :diff diff
      :commit commit
      :diff-str diff-str)))

(defn- diff-div
  [color k v]
  [:div (str k " ") [:a {:style {:background color
                                 :width      :max-content}} v]])

(defn seq-merge
  [a v]
  (let [empty-diff (map (fn [p m]
                          (cond-> nil
                            p (assoc :+ p)
                            m (assoc :- m))) (:+ v) (:- v))]
    (loop [e empty-diff
           original a
           acc []]
      (cond (empty? e) acc
            (nil? (first e)) (recur (rest e) (rest original) (conj acc (first original)))
            (map? (:- (first e))) (recur (rest e) (rest original) (conj acc (prepare-print (first original) (first e))))
            (:- (first e)) (recur (rest e) (rest original) (conj acc (first e)))
            :else (recur (rest e) original (conj acc (first e)))))))

(defn table-row
  []
  ^{:key (gensym)} [:tr
                    {:style {:border-bottom   "5px"
                             :border-top      "5px"
                             :border-style    :solid
                             :text-align      :center
                             :border-collapse :collapse}}])

(defn table
  [body]
  [:div [:div {:style {:overflow     :auto
                       :border-style :solid
                       :border-width :thin
                       :border-color :gray}}
         [:table {:style {:cellpadding "0px"}}
          [:tbody body]]]])

(defn td-border
  ([]
   ^{:key (gensym)} [:td {:style {:border-bottom :solid
                                  :border-width  :thin}}])
  ([color content]
   ^{:key (gensym)} [:td {:style {:border-bottom :solid
                                  :border-width  :thin
                                  :background    color}} content]))

(defn td
  ([]
   ^{:key (gensym)} [:td])
  ([content]
   ^{:key (gensym)} [:td content])
  ([color content]
   ^{:key (gensym)} [:td {:style {:background color}} content]))

(defn- colorize-core
  [a-value diff]
  (letfn [(color-seq
            [s]
            (table
              ^{:key (gensym)}
              [:tr (td "(")
               (into (table-row)
                     (for [c s]
                       (cond
                         (and (coll? c) (nil? (:- c)) (nil? (:+ c))) (td (colorize-core (a-value (.indexOf (vec s) c)) {:to-print c}))
                         (and (coll? c) (map? (:- c)) (map? (:+ c))) (td (colorize-core (a-value (.indexOf (vec s) c)) {:to-print c}))
                         (:- c) (td :lightcoral (str (:- c)))
                         (not (:+ c)) (td :lightgrey (str c))
                         :else (td))))
               (into (table-row)
                     (for [c s]
                       (cond
                         (:+ c) (td :lightgreen (str (:+ c)))
                         :else (td))))
               (td ")")]))
          (color-map2
            [a m]
            (table
              ^{:key (gensym)}
              [:tr (td "{")
               (mapcat distinct
                       (for [[k v] m]
                         [(into (table-row)
                                (cond
                                  (and (map? v) (every? coll? [(:- v) (:+ v)])) [(td-border :white (str k))
                                                                                 (td :white (colorize-core (get a k) {:to-print (seq-merge (get a-value k) v)}))]
                                  (and (:- v) (:+ v)) [(td :white (str k))
                                                       (td :lightcoral (str (:- v)))]
                                  (:- v) [(td-border :lightcoral (str k))
                                          (td-border :lightcoral (str (:- v)))]
                                  (and (map? v) (every? nil? [(:- v) (:+ v)])) [(td-border :while (str k))
                                                                                (td :white (colorize-core (get a k) {:to-print v}))]
                                  (every? nil? [(:- v) (:+ v)]) [(td-border :lightgrey (str k))
                                                                 (td-border :lightgrey (str v))]))
                          (into (table-row)
                                (cond
                                  (and (map? v) (every? coll? [(:- v) (:+ v)])) [[:td]]
                                  (and (nil? (:- v)) (:+ v)) [(td :lightgreen (str k))
                                                              (td :lightgreen (str (:+ v)))]
                                  (:+ v) [(td-border)
                                          (td-border :lightgreen (str (:+ v)))]))]))

               (td "}")]))]
    (if
      (map? (:to-print diff))
      (color-map2 a-value (:to-print diff))
      (color-seq (:to-print diff)))))

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
  [{:keys [a-value diff]}]
  (let [c (colorize-core a-value diff)]
    (if (not-empty diff)
      (into (visual-div "")
            (rest c))
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
  (let [{:keys [a-input b-input]} @state
        colorized (colorize @state)]
    [:div {:style {:margin :auto
                   :width  :max-content}}
     [:table {:style {:margin :auto
                      :width  :max-content}}
      [:thead ^{:key (gensym)} [:tr
                                ^{:key (gensym)} [:th "Map A"]
                                ^{:key (gensym)} [:th "Map B"]
                                ^{:key (gensym)} [:th "Calculated diff"]
                                ^{:key (gensym)} [:th "Visual diff"]
                                ^{:key (gensym)} [:th "Commit diff on Map A"]]]
      [:tbody [:tr
               [:td (text-area a-input (partial on-change state :a-input))]
               [:td (text-area b-input (partial on-change state :b-input))]
               [:td (text-area (get @state :diff-str "No diff calculated"))]
               [:td {:style {:vertical-align :top}} colorized]
               [:td (text-area (get @state :commit "No diff calculated"))]]]]
     [:p (str (:error @state))]]))
