(ns ambition.core
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]))

(enable-console-print!)

(def app-state
  (atom {}))

(om/root
  (fn [app owner]
    (om/component (dom/h1 nil (:text app))))
  app-state
  {:target (. js/document (getElementById "app"))})
