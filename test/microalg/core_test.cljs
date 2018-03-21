(ns microalg.core-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.core :refer (evaluate-str)]))

(deftest arithmetics-test
    (are [expected actual] (= expected actual)
         3 (evaluate-str "(+ 1 2)")
         6 (evaluate-str "(+ 1 (+ 2 3))")))
