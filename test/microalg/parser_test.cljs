(ns microalg.parser-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.parser :refer [parser]]))

(deftest sexpr-test
  (are [expected actual] (= expected actual)
       1 (parser "1")
       'x (parser "x")
       '(+ 2 2) (parser "(+ 2 2)")
       ))
