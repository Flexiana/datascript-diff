(ns ui
  (:require
    [diff-ui :refer [diff-ui state]]
    [differences :as difference]
    [nubank.workspaces.card-types.react :refer [react-card]]
    [nubank.workspaces.core :refer [mount defcard]]
    [nubank.workspaces.model :as wsm]
    [reagent.core :as r]
    [reagent.dom :as dom]))


(defcard diff-card (react-card (r/as-element [diff-ui state])))


(defcard differences-card
  {::wsm/align {:flex 1}}
  (react-card (r/as-element [difference/ui difference/*editor])))


(defonce run
  (mount))
