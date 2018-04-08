(ns microalg.core
    (:require [jquery]
              [jquery.terminal]))

(def app-div (js/$ "#app"))
(.html app-div "")
(def term (.terminal app-div))
