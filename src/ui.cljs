(ns ui
  (:require [reagent.dom :as dom]
            [recursive-ui :refer [diff]]
            [reagent.core :as r]))

(defn run []
  (dom/render
   [diff (r/atom {})]
   (js/document.getElementById "app")))
