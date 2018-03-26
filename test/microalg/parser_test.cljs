(ns microalg.parser-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.parser :refer [parser]]))

(deftest sexpr-test
  (are [actual expected] (= actual expected)
       (parser "1")
       1
       (parser "x")
       'x
       (parser "(+ 2 2)")
       '(+ 2 2)
       (parser "(+ 1 (+ 2 3))")
       '(+ 1 (+ 2 3))))

(deftest string-test
  (are [actual expected] (= actual expected)
       (parser "(Afficher \"Bonjour tout le monde !\")")
       '(Afficher "Bonjour tout le monde !")
       (parser "(Afficher \"Avec un \\\" pour voir\")")
       '(Afficher "Avec un \" pour voir")
       (parser "(Afficher \"Avec un \\\\ aussi pour voir\")")
       '(Afficher "Avec un \\ aussi pour voir")))

(deftest errors-test
  (are [actual expected] (= actual expected)
       (parser "1 2")
       {:type :parse :line 1 :column 1
        :info ["opening parenthesis" "number" "string" "symbol"]}
       (parser "(")
       {:type :parse :line 1 :column 2
        :info ["opening parenthesis" "number" "whitespace" "string" "symbol"]}
       (parser ")")
       {:type :parse :line 1 :column 1
        :info ["opening parenthesis" "number" "string" "symbol"]}
       (parser "()")
       {:type :parse :line 1 :column 2
        :info ["opening parenthesis" "number" "whitespace" "string" "symbol"]}
       (parser "(+")
       {:type :parse :line 1 :column 3
        :info ["closing parenthesis" "whitespace"]}
       (parser "(+ 2")
       {:type :parse :line 1 :column 5
        :info ["closing parenthesis" "whitespace"]}
       (parser "(+ 2 2")
       {:type :parse :line 1 :column 7
        :info ["closing parenthesis" "whitespace"]}))
