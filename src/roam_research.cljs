(ns roam-research)

(defn ->clj [s]
  (js->clj (.parse js/JSON s) :keywordize-keys false))
