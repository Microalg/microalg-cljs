(ns microalg.interp
    (:require [cljs.core.match :refer-macros [match]]
              [microalg.parser]))

(declare interps env-base-value default-interp-data
         eprogn eq? evlis extend  ; not a Clojure fn, bad highlighting
         invoke lookup make-function update! wrong)

; « the book » means Lisp in Small Pieces
; atom? and pair? are not in Clojure (many more types than atom vs pairs)
(def atom? #(or (not (coll? %)) (empty? %)))
(def pair? (complement atom?))
; emulate old style
(def car first)
(def caar (comp first first))
(def cadr second)
(def caddr #(nth % 2))  ; third
(def cadddr #(nth % 3)) ; fourth
(def cdr rest)
(def cdar (comp rest first))
(def cddr (comp rest rest))
; we use do for begin

(defn extract-env
  [interp-key new-vars]
  (merge (-> @interps (get interp-key) :env) new-vars))

(defn evaluate
  [exp [interp-key new-vars]]  ; the book uses e instead of exp
  (let [env (extract-env interp-key new-vars)]
    (if (atom? exp)
      (if (symbol? exp)
        (lookup exp [interp-key new-vars])
        exp)  ; no check here but some are in the book
      (case (car exp)
        Brut
          (cadr exp)
        Si
          (if (not (eq? (evaluate (cadr exp) [interp-key new-vars]) 'Faux))
            (evaluate (caddr exp) [interp-key new-vars])
            (evaluate (cadddr exp) [interp-key new-vars]))
        Bloc
          (eprogn (cdr exp) [interp-key new-vars])
        Affecter_a
          (update! (cadr exp)
                   [interp-key new-vars]
                   (evaluate (caddr exp) [interp-key new-vars]))
        Fonction
          (make-function [interp-key new-vars]
                         {:pos (meta exp)
                          :variables (cadr exp)
                          :body (cddr exp)})
        RAZ_environnement
          (do (swap! interps assoc-in [interp-key :env] env-base-value)
              'Rien)
        (let [f-expr (car exp)
              f-val  (evaluate (car exp) [interp-key new-vars])
              args   (evlis (cdr exp) [interp-key new-vars])]
          (invoke f-expr f-val args))))))

(defn safe-evaluate
  [exp [interp-key new-vars]]
  (try
    (evaluate exp [interp-key new-vars])
    (catch js/Object e e)))

(defn evaluate-str
  ([src]
   (evaluate-str src ["default" {}]))
  ([src [interp-key new-vars]]
   (when (not (some #{interp-key} (keys @interps)))
     (swap! interps assoc interp-key default-interp-data))
   (let [result (microalg.parser/parser src)]
     (match result
      [:sexpr sexpr] (safe-evaluate sexpr [interp-key new-vars])
      ; at this point the result should be an error, we forward it
      :else result))))

(defn eprogn
  [exps [interp-key new-vars]]
  (let [env (extract-env interp-key new-vars)]
    (if (pair? exps)
      (if (pair? (cdr exps))
        (do
          (evaluate (car exps) [interp-key new-vars])
          (eprogn (cdr exps) [interp-key new-vars]))
        (evaluate (car exps) [interp-key new-vars]))
      'Rien )))

(defn evlis
  [exps [interp-key new-vars]]
  (let [env (extract-env interp-key new-vars)]
    (if (pair? exps)
      ; this version forces left to right eval
      (let [argument1 (evaluate (car exps) [interp-key new-vars])]
           (cons argument1 (evlis (cdr exps) [interp-key new-vars]))); !!! [] vs ()
      [])))

(defn invoke
  [expr fun args]  ; changed `fn` to `fun` and added `expr` for error msg
  (if (fn? fun)
    (try
      (apply fun args)
      (catch js/Object e
        (match e
          [:incorrect-arity expected-arity arity args]
            (wrong :incorrect-arity (meta expr) expected-arity arity args)
          :else e)))
    (wrong :not-a-function (meta expr) (str expr))))

(defn make-function
  [[interp-key new-vars] {:keys [pos variables body]}]
  (fn [& values]
    (eprogn body (extend [interp-key new-vars]
                         {:pos       pos
                          :variables variables
                          :values    values}))))

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
  (fn [& args]
    (if (= arity (count args))
      (apply fun args)  ; The real apply of Clojure
      ; not like in the book: should be caught in `invoke`
      ; that's why we do not use `wrong` but `throw` directly.
      (throw [:incorrect-arity arity (count args) args]))))

(def interps (atom {}))

(def env-base-value
  {'Rien 'Rien
   'Vrai 'Vrai
   'Faux 'Faux
   'foo 'Rien
   '+ (make-prim "+" + 2)
   })

(def default-interp-data
  {:env env-base-value
   :runs []})

(defn lookup
  [id [interp-key new-vars]]
  (let [env (extract-env interp-key new-vars)
        value (id env)]
    (if (nil? value)  ; nil can't be a value in MicroAlg
      (wrong :no-such-binding (meta id) (str id)))
      value))

; TODO: lock Rien Vrai Faux and primitives
(defn update!
  [id [interp-key new-vars] value]
  (let [env (extract-env interp-key new-vars)
        oldvalue (id env)]
    (if (nil? oldvalue)  ; nil can't be a value in MicroAlg
      (wrong :no-such-binding (meta id) (str id))
      (do
        (swap! interps assoc-in [interp-key :env id] value)
        'Rien))))  ; return value of an assignment

(defn wrong
  [tag pos-info & args]
  (throw
    ; OPTIMIZE: those keywords should appear in the let.
    [:eval-error {:start-line   (:instaparse.gll/start-line pos-info)
                  :start-column (:instaparse.gll/start-column pos-info)
                  :end-line     (:instaparse.gll/end-line pos-info)
                  :end-column   (:instaparse.gll/end-column pos-info)
                  :info (apply vector tag args)}]))

(defn extend
  [[interp-key new-vars] {:keys [pos variables values]}]
  (let [env (extract-env interp-key new-vars)
        num-vars (count variables)
        num-vals (count values)]
    (cond
      (not= num-vars num-vals)
        (wrong :incorrect-arity pos num-vars num-vals values)
      :else
        (let [var-val-pairs (map vector variables values)]
          [interp-key (into new-vars var-val-pairs)]))))
