(ns microalg.repl
    (:require [microalg.core :as malg]))

(enable-console-print!)

(def readline (js/require "readline"))
(def rl (.createInterface readline #js {:input (.-stdin js/process)
                                        :output (.-stdout js/process)}))
(defn toplevel
  []
  (.question rl "> " #(do (println (malg/evaluate-str % malg/env-global))
                          (toplevel))))

(defn -main
  []
  (toplevel))
