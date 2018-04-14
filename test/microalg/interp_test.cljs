(ns microalg.interp-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.interp :refer (evaluate-str env-global)]))

(deftest arithmetics-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 1 2)" env-global)
       3
       (evaluate-str "(+ 1 (+ 2 3))" env-global)
       6))

(deftest vars-test
  (are [actual expected] (= actual expected)
       (evaluate-str "foo" env-global)
       'Rien
       (evaluate-str "(Affecter_a foo 1)" env-global)
       'Rien
       (evaluate-str "foo" env-global)
       1
       (evaluate-str "(RAZ_environnement)" env-global)
       'Rien
       (evaluate-str "foo" env-global)
       'Rien))

(deftest parse-propagation-errors-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 2 2" env-global)
       [:parse-error
        {:line 1 :column 7
         :info ["closing parenthesis" "whitespace"]}]))

(deftest eval-errors-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 1 2 3)" env-global)
       [:eval-error
        {:start-line 1 :start-column 2
         :end-line 1 :end-column 3
         :info [:incorrect-arity "+" 2 3 '(1 2 3)]}]
       (evaluate-str "(+ 1 2 (+ 3 4))" env-global)
       [:eval-error
        {:start-line 1 :start-column 2
         :end-line 1 :end-column 3
         :info [:incorrect-arity "+" 2 3 '(1 2 7)]}]
       (evaluate-str "(+ 1 (foo 2 3))" env-global)
       [:eval-error
        {:start-line 1 :start-column 7
         :end-line 1 :end-column 10
         :info [:not-a-function "foo"]}]
       (evaluate-str "(Affecter_a x 1)" env-global)
       [:eval-error
        {:start-line 1 :start-column 13
         :end-line 1 :end-column 14
         :info [:no-such-binding "x"]}]
       (evaluate-str "(+ 1 (-- 2 3))" env-global)
       [:eval-error
        {:start-line 1 :start-column 7
         :end-line 1 :end-column 9
         :info [:no-such-binding "--"]}]))
