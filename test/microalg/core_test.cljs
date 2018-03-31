(ns microalg.core-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.core :refer (evaluate-str evaluate env-global)]))

(deftest arithmetics-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 1 2)" env-global)
       3
       (evaluate-str "(+ 1 (+ 2 3))" env-global)
       6))

(deftest parse-propagation-errors-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 2 2" env-global)
       [:parse-error
        {:line 1 :column 7
         :info ["closing parenthesis" "whitespace"]}]))

(deftest eval-errors-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 1 (foo 2 3))" env-global)
       [:eval-error
        {:start-line 1 :start-column 7
         :end-line 1 :end-column 10
         :info [:not-a-function "foo"]}]
       (evaluate-str "(+ 1 (-- 2 3))" env-global)
       [:eval-error
        {:start-line 1 :start-column 7
         :end-line 1 :end-column 9
         :info [:no-such-binding "--"]}]))
