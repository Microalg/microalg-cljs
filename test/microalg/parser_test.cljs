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
       (parser "(Afficher \"Bonjour tout le monde !\")")
       [:sexpr '(Afficher "Bonjour tout le monde !")]
       (parser "(Afficher \"Avec un \\\" pour voir\")")
       [:sexpr '(Afficher "Avec un \" pour voir")]
       (parser "(Afficher \"Avec un \\\\ aussi pour voir\")")
       [:sexpr '(Afficher "Avec un \\ aussi pour voir")]))

(deftest errors-test
  (are [actual expected] (= actual expected)
       (parser "1 2")
       [:parse-error {:line 1 :column 1
        :info ["opening parenthesis" "number" "string" "symbol"]}]
       (parser "(")
       [:parse-error {:line 1 :column 2
        :info ["opening parenthesis" "number" "whitespace" "string" "symbol"]}]
       (parser ")")
       [:parse-error {:line 1 :column 1
        :info ["opening parenthesis" "number" "string" "symbol"]}]
       (parser "()")
       [:parse-error {:line 1 :column 2
        :info ["opening parenthesis" "number" "whitespace" "string" "symbol"]}]
       (parser "(+")
       [:parse-error {:line 1 :column 3
        :info ["closing parenthesis" "whitespace"]}]
       (parser "(+ 2")
       [:parse-error {:line 1 :column 5
        :info ["closing parenthesis" "whitespace"]}]
       (parser "(+ 2 2")
       [:parse-error {:line 1 :column 7
        :info ["closing parenthesis" "whitespace"]}]))
