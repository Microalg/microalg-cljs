(ns microalg.parser-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.parser :refer [parser]]))

(deftest sexpr-test
  (are [expected actual] (= expected actual)
       1 (parser "1")
       'x (parser "x")
       '(+ 2 2) (parser "(+ 2 2)")
       '(+ 1 (+ 2 3)) (parser "(+ 1 (+ 2 3))")))

(deftest string-test
  (are [expected actual] (= expected actual)
       '(Afficher "Bonjour tout le monde !")
         (parser "(Afficher \"Bonjour tout le monde !\")")
       '(Afficher "Avec un \" pour voir")
         (parser "(Afficher \"Avec un \\\" pour voir\")")
       '(Afficher "Avec un \\ aussi pour voir")
         (parser "(Afficher \"Avec un \\\\ aussi pour voir\")")))

(deftest errors-test
  (are [expected actual] (= expected actual)
       {:type :parse :line 1 :column 1
        :info ["opening parenthesis" "number" "string" "symbol"]}
         (parser "1 2")
       {:type :parse :line 1 :column 2
        :info ["opening parenthesis" "number" "whitespace" "string" "symbol"]}
         (parser "(")
       {:type :parse :line 1 :column 1
        :info ["opening parenthesis" "number" "string" "symbol"]}
         (parser ")")
       {:type :parse :line 1 :column 2
        :info ["opening parenthesis" "number" "whitespace" "string" "symbol"]}
         (parser "()")
       {:type :parse :line 1 :column 3
        :info ["closing parenthesis" "whitespace"]}
         (parser "(+")
       {:type :parse :line 1 :column 5
        :info ["closing parenthesis" "whitespace"]}
         (parser "(+ 2")
       {:type :parse :line 1 :column 7
        :info ["closing parenthesis" "whitespace"]}
         (parser "(+ 2 2")))
