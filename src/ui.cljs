(ns ui
  (:require [reagent.dom :as dom]))

(defn run []
  (dom/render
   [:div "Hi"]
   (js/document.getElementById "app")))
