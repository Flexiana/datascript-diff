(ns ui
  (:require [reagent.dom :as dom]
            [nubank.workspaces.card-types.react :refer [react-card]]
            [nubank.workspaces.core :refer [mount defcard]]
            [diff-ui :refer [diff state]]
            [reagent.core :as r]))

#_(defn run []
    (dom/render
     [diff (r/atom {})]
     (js/document.getElementById "app")))

(defcard diff-card (react-card (r/as-element [diff state])))

(defonce run (mount))
