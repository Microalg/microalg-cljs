(ns microalg.parser
    (:require [instaparse.core :as insta :refer-macros [defparser]]
              [cljs.reader :refer [read-string]]))

; https://stackoverflow.com/questions/18187249/how-do-we-define-a-grammar-for-clojure-code-using-instaparse
(defparser grammar
  "<src> = atom | operation
   <atom> = num / str / sym
   num = #'[0-9]+'
   str = '\"' '\"'
   sym = #'[a-zA-Z0-9_=+-/*~|{}^!?#$%&\\'`]*'
   <operation> = <'('> wspace* src (wspace+ src)* wspace* <')'>
   <wspace>  = <#'\\s'>")

(defn read-num
  [src]
  (read-string src))

(defn parser
  [src]
  (insta/transform
    {:num read-num :str identity :sym symbol}
    (grammar src)))
