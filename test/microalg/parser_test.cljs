(ns microalg.parser-test
    (:require [cljs.test :refer-macros [deftest are]]
              [microalg.parser :refer [parser]]))

(deftest sexpr-test
  (are [expected actual] (= expected actual)
       ;[:sexp [:number "1"]] (parser "1")
       [:sexp [:operation [:operator "+"]
                          [:args [:sarg [:arg "2"]] [:sarg [:arg "2"]]]]]
       (parser "(+ 2 2)")
       ))
