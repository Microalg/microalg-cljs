(ns microalg.parser-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.parser :refer [parser]]))

(deftest sexpr-test
  (are [actual expected] (= actual expected)
       (parser "1")
       [:sexpr 1]
       (parser "x")
       [:sexpr 'x]
       (parser "(+ 2 2)")
       [:sexpr '(+ 2 2)]
       (parser "(+ 1 (+ 2 3))")
       [:sexpr '(+ 1 (+ 2 3))]))

(deftest string-test
  (are [actual expected] (= actual expected)
       (parser "\"Bonjour tout le monde !\"")
       [:sexpr "Bonjour tout le monde !"]
       (parser "\"Avec un \\\" pour voir\"")
       [:sexpr "Avec un \" pour voir"]
       (parser "\"Avec un \\\\ aussi pour voir\"")
       [:sexpr "Avec un \\ aussi pour voir"]))

(deftest errors-test
  (are [actual expected] (= actual expected)
       (parser "1 2")
       [:parse-error
        {:line 1 :column 1
         :info ["opening parenthesis" "number" "string" "symbol"]}]
       (parser "(")
       [:parse-error
        {:line 1 :column 2
         :info ["opening parenthesis" "symbol" "whitespace"]}]
       (parser ")")
       [:parse-error
        {:line 1 :column 1
         :info ["opening parenthesis" "number" "string" "symbol"]}]
       (parser "(1 2 3)")
       [:parse-error
        {:line 1 :column 2
         :info ["opening parenthesis" "symbol" "whitespace"]}]
       (parser "(\"salut\" 2 3)")
       [:parse-error
        {:line 1 :column 2
         :info ["opening parenthesis" "symbol" "whitespace"]}]
       (parser "()")
       [:parse-error
        {:line 1 :column 2
         :info ["opening parenthesis" "symbol" "whitespace"]}]
       (parser "(+")
       [:parse-error
        {:line 1 :column 3
         :info ["closing parenthesis" "whitespace"]}]
       (parser "(+ ")
       [:parse-error
        {:line 1 :column 4
         :info ["closing parenthesis" "whitespace" "opening parenthesis" "number" "string" "symbol"]}]
       (parser "(+ 2 2")
       [:parse-error
        {:line 1 :column 7
        :info ["closing parenthesis" "whitespace"]}]))
