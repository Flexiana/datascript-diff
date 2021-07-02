(ns roam-research)

(defn ->clj [s]
  #?(:cljs (js->clj (.parse js/JSON s) :keywordize-keys false)))
