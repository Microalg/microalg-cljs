(ns microalg.core-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.core :refer (evaluate env-global)]))

(deftest arithmetics-test
    (are [expected actual] (= expected actual)
         3 (evaluate '(+ 1 2) env-global)
         6 (evaluate '(+ 1 (+ 2 3)) env-global)))
