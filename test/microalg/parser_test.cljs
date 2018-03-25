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
