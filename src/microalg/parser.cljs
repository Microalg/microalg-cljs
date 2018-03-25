(ns microalg.parser
    (:require [instaparse.core :as insta :refer-macros [defparser]]
              [clojure.string :as str]
              [cljs.reader :refer [read-string]]))

; parser:
; https://stackoverflow.com/questions/18187249/how-do-we-define-a-grammar-for-clojure-code-using-instaparse
; string regex:
; https://stackoverflow.com/questions/481282/how-can-i-match-double-quoted-strings-with-escaped-double-quote-characters
(defparser grammar
  "<src> = atom | operation
   atom = num / str / sym
   num = #'[0-9]+'
   str = #'\"([^\\\"]|\\\\|\\\")*\"'
   sym = #'[a-zA-Z0-9_=+-/*~|{}^!?#$%&\\'`]*'
   operation = <'('> wspace* src (wspace+ src)* wspace* <')'>
   <wspace>  = <#'\\s'>")

(defn read-num
  [src]
  (read-string src))

(defn read-str
  [src]
  (let [quotes-removed (apply str (rest (drop-last src)))]
    (-> quotes-removed
        (str/replace  "\\\\" "\\")
        (str/replace  "\\\"" "\""))))

(defn parser
  [src]
  (insta/transform
    {:atom identity :operation list
     :num read-num :str read-str :sym symbol}
    (first (grammar src))))
