(ns microalg.core
    (:require [jquery]
              [jquery.terminal]
              [microalg.interp :as interp]))

(def app-div (js/$ "#app"))
(def options {:clear  false
              :exit   false
              :prompt ":> "})

(.html app-div "")
(def term (.terminal app-div
                     #(clj->js (interp/evaluate-str %))
                     (clj->js options)))
(.clear term)
(.echo term "MicroAlg")
