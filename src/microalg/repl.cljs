(ns microalg.repl
    (:require [microalg.interp :as interp]))

(enable-console-print!)

(def readline (js/require "readline"))
(def rl (.createInterface readline #js {:input (.-stdin js/process)
                                        :output (.-stdout js/process)}))
(defn toplevel
  []
  (.question rl "> " #(do (println (interp/evaluate-str % interp/env-global))
                          (toplevel))))

(defn -main
  []
  (toplevel))

(set! *main-cli-fn* -main)
