(ns microalg.parser
    (:require [instaparse.core :refer-macros [defparser]]))

; https://stackoverflow.com/questions/18187249/how-do-we-define-a-grammar-for-clojure-code-using-instaparse
(defparser parser
  "sexp = lparen space operation space rparen
   <lparen> = <'('>
   <rparen> = <')'>
   operation = operator + args
   operator = '+'
   args = sarg+
   sarg = space arg
   arg = #'[0-9]+'
   <space>  = <#'[ ]*'>
  "
  )
