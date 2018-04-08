(ns microalg.core
    (:require [jquery]
              [jquery.terminal]))

(def term (.terminal (js/$ "#app")))
