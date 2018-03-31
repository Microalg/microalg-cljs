(ns microalg.core
    (:require [cljs.core.match :refer-macros [match]]
              [microalg.parser :refer [parser]]))

; « the book » means Lisp in Small Pieces
; atom? and pair? are not in Clojure (many more types than atom vs pairs)
(def atom? #(or (not (coll? %)) (empty? %)))
(def pair? (complement atom?))
; emulate old style
(def car first)
(def caar (comp first first))
(def cadr second)
(def caddr #(nth % 3))
(def cadddr #(nth % 4))
(def cdr rest)
(def cdar (comp rest first))
(def cddr (comp rest rest))
; we use do for begin

(declare env-global eprogn eq? evlis extend  ; not a Clojure fn, bad highlighting
         invoke lookup make-function update! wrong)

(defn evaluate
  [exp env]  ; the book uses e instead of exp
  (if (atom? exp)
    (if (symbol? exp)
      (lookup exp env)
      exp)  ; no check here but some are in the book
    (case (car exp)
      Brut
        (cadr exp)
      Si
        (if (not (eq? (evaluate (cadr exp) env) 'Faux))
          (evaluate (caddr exp) env)
          (evaluate (cadddr exp) env))
      Bloc
        (eprogn (cdr exp) env)
      Affecter_a
        (update! (cadr exp) env (evaluate (caddr exp) env))
      Fonction
        (make-function (cadr exp) (cddr exp) env)
      (invoke (car exp) (evaluate (car exp) env) (evlis (cdr exp) env)))))

(defn safe-evaluate
  [exp env]
  (try
    (evaluate exp env)
    (catch js/Object e e)))

(defn evaluate-str
  [src env]
  (let [result (parser src)]
    (match result
      [:sexpr sexpr] (safe-evaluate sexpr env)
      ; at this point the result should be an error, we forward it
      :else result)))

(defn eprogn
  [exps env]
  (if (pair? exps)
    (if (pair? (cdr exps))
      (do
        (evaluate (car exps) env)
        (eprogn (cdr exps) env)
        (evaluate (car exps) env))
      'Rien )))

(defn evlis
  [exps env]
  (if (pair? exps)
    ; this version forces left to right eval
    (let [argument1 (evaluate (car exps) env)]
                    (cons argument1 (evlis (cdr exps) env))); !!! [] vs ()
    []))

(defn invoke
  [expr fun args]  ; changed `fn` to `fun` and added `expr` for error msg
  (if (fn? fun)
    (fun args)
    (wrong :not-a-function expr (str expr))))

(defn make-function
  [variables body env]
  (fn [values] (eprogn body (extend env variables values))))

; Regarding the env:
; * we don't use a A-list but a simple map
; * implementation of lookup, update!, extend are different than in the book
; * we don't need `set-cdr!` anymore!
; * we don't def an `env-init` to be filled to `env-global` but def the
;   latter right away
; * we don't need `definitial` or `defprimitive`, but `make-prim`

; utility function, like defprimitive but renamed value->fun values->args
(defn make-prim
  [name fun arity]
  (fn [args]
    (if (= arity (count args))
      (apply fun args)  ; The real apply of Clojure
      (wrong "Incorrect arity" (list name args)))))

(def env-global
  {'Rien 'Rien
   'Vrai 'Vrai
   'Faux 'Faux
   'foo 'Rien
   '+ (make-prim "+" + 2)
  })

(defn lookup
  [id env]
  (let [value (id env)]
    (if (nil? value)  ; nil can't be a value in MicroAlg
      (wrong :no-such-binding id (str id)))
      value))

; TODO: lock Rien Vrai Faux and primitives
(defn update!
  [id env value]
  (let [oldvalue (id env)]
    (if (nil? value)  ; nil can't be a value in MicroAlg
      (wrong :no-such-binding id)
      (do
        (swap! env #(update % id (constantly value)))
        'Rien))))  ; return value of an assignment

(defn wrong
  [tag expr-with-pos-as-meta & args]
  (let [pos-as-meta (meta expr-with-pos-as-meta)]
    (throw
      ; OPTIMIZE: those keywords should appear in the let.
      [:eval-error {:start-line   (:instaparse.gll/start-line pos-as-meta)
                    :start-column (:instaparse.gll/start-column pos-as-meta)
                    :end-line     (:instaparse.gll/end-line pos-as-meta)
                    :end-column   (:instaparse.gll/end-column pos-as-meta)
                    :info (apply vector tag args)}])))

(defn extend
  [env variables values]
  (cond
    (symbol? variables)
      (assoc env variables values)
    (coll? variables)
      (let [num-vars (count variables)
            num-vals (count values)]
        (cond
          (> num-vars num-vals)
            (wrong "Too less values")
          (< num-vars num-vals)
            (wrong "Too much values")
          :else (apply assoc env (map vector variables values))))
    :else (wrong "Cannot handle this env extension")))
