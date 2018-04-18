(ns microalg.interp-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.interp :refer (evaluate-str)]))

(deftest arithmetics-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 1 2)")
       3
       (evaluate-str "(+ 1 (+ 2 3))")
       6))

(deftest vars-test
  (are [actual expected] (= actual expected)
       (evaluate-str "foo")
       'Rien
       (evaluate-str "(Affecter_a foo 1)")
       'Rien
       (evaluate-str "foo")
       1
       (evaluate-str "(RAZ_environnement)")
       'Rien
       (evaluate-str "foo")
       'Rien))

(deftest fun-def-test
  (are [actual expected] (= actual expected)
       (evaluate-str "((Fonction (x) (Affecter_a foo x)) 2)")
       'Rien
       (evaluate-str "foo")
       2))

(deftest parse-propagation-errors-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 2 2")
       [:parse-error
        {:line 1 :column 7
         :info ["closing parenthesis" "whitespace"]}]))

(deftest eval-errors-test
  (are [actual expected] (= actual expected)
       (evaluate-str "(+ 2)")
       [:eval-error
        {:start-line 1 :start-column 2
         :end-line 1 :end-column 3
         :info [:incorrect-arity "+" 2 1 '(2)]}]
       (evaluate-str "(+ 1 2 3)")
       [:eval-error
        {:start-line 1 :start-column 2
         :end-line 1 :end-column 3
         :info [:incorrect-arity "+" 2 3 '(1 2 3)]}]
       (evaluate-str "(+ 1 (+ 2 3 4))")
       [:eval-error
        {:start-line 1 :start-column 7
         :end-line 1 :end-column 8
         :info [:incorrect-arity "+" 2 3 '(2 3 4)]}]
       (evaluate-str "(+ 1 (foo 2 3))")
       [:eval-error
        {:start-line 1 :start-column 7
         :end-line 1 :end-column 10
         :info [:not-a-function "foo"]}]
       (evaluate-str "(Affecter_a x 1)")
       [:eval-error
        {:start-line 1 :start-column 13
         :end-line 1 :end-column 14
         :info [:no-such-binding "x"]}]
       (evaluate-str "(+ 1 (-- 2 3))")
       [:eval-error
        {:start-line 1 :start-column 7
         :end-line 1 :end-column 9
         :info [:no-such-binding "--"]}]))
