(ns ui
  (:require [reagent.dom :as dom]
            [nubank.workspaces.card-types.react :refer [react-card]]
            [nubank.workspaces.core :refer [mount defcard]]
            [diff-ui :refer [diff-ui state]]
            [reagent.core :as r]))

#_(defn run []
    (dom/render
     [diff-ui (r/atom {})]
     (js/document.getElementById "app")))

(defcard diff-card (react-card (r/as-element [diff-ui state])))

(defonce run (mount))
