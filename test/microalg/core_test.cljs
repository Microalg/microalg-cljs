(ns microalg.core-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.core :refer (evaluate env-global)]))

(deftest arithmetics-test
  (are [actual expected] (= actual expected)
       (evaluate '(+ 1 2) env-global)
       3
       (evaluate '(+ 1 (+ 2 3)) env-global)
       6))
