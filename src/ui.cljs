(ns ui
  (:require [reagent.dom :as dom]
            [nubank.workspaces.card-types.react :refer [react-card]]
            [nubank.workspaces.core :refer [mount defcard]]
            [diff-ui :refer [diff state]]
            [differences :as difference]
            [reagent.core :as r]))

(defcard diff-card
  (react-card (r/as-element [diff state])))

(defcard differences-card
  (react-card (r/as-element [difference/ui difference/*editor])))

(defonce run (mount))
