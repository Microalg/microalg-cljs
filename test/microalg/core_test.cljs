(ns microalg.core-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.core :refer (evaluate-str evaluate env-global)]))

(deftest arithmetics-test
  (are [actual expected] (= actual expected)
       (evaluate '(+ 1 2) env-global)
       3
       (evaluate '(+ 1 (+ 2 3)) env-global)
       6))

(deftest evaluate-str-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 2 2)" env-global)
       4))

(deftest parse-propagation-errors-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 2 2" env-global)
       [:parse-error
        {:line 1 :column 7
         :info ["closing parenthesis" "whitespace"]}]))
