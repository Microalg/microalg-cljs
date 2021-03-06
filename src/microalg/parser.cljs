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
   sym = #'[a-zA-Z_=+-/*~|{}^!?#$%&\\'`]+'
   operation = <'('> wspace* (sym | operation) (wspace+ src)* wspace* <')'>
   <wspace>  = <#'\\s'>")

(defn read-num
  [src]
  ; TODO: clever number representation
  (read-string src))

(def strip-first-and-last-chars #(apply str (rest (drop-last %))))

(def escape-backslashes #(-> % (str/replace  "\\\\" "\\")
                               (str/replace  "\\\"" "\"")))

(def read-str (comp escape-backslashes strip-first-and-last-chars))

(defn pretty-expecting
  [expected]
  (case (str expected)
    "(" "opening parenthesis"
    ")" "closing parenthesis"
    "/^\\s/" "whitespace"
    "/^[0-9]+/" "number"
    "/^\\\"([^\\\"]|\\\\|\\\")*\\\"/" "string"
    "/^[a-zA-Z_=+-/*~|{}^!?#$%&'`]+/" "symbol"
    (str expected)))

(defn parser
  [src]
  (let [result (grammar src)]
    (if (insta/failure? result)
      (let [{:keys [line column reason]} (insta/get-failure result)]
        [:parse-error
         {:line line
          :column column
          :info (->> reason (map :expecting)
                            (map pretty-expecting))}])
      (let [with-pos (insta/add-line-and-column-info-to-metadata src result)]
        [:sexpr
         (insta/transform
           {:atom identity :operation list
            :num read-num :str read-str :sym symbol}
           (first with-pos))]))))
