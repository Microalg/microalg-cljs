(ns microalg.interp
    (:require [cljs.core.match :refer-macros [match]]
              [microalg.parser]))

(declare env-global-base-value env-global
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

(defn evaluate
  [exp env conf]  ; the book uses e instead of exp
  (if (atom? exp)
    (if (symbol? exp)
      (lookup exp env)
      exp)  ; no check here but some are in the book
    (case (car exp)
      Brut
        (cadr exp)
      Si
        (if (not (eq? (evaluate (cadr exp) env conf) 'Faux))
          (evaluate (caddr exp) env conf)
          (evaluate (cadddr exp) env conf))
      Bloc
        (eprogn (cdr exp) env)
      Affecter_a
        (update! (cadr exp) env (evaluate (caddr exp) env conf))
      Fonction
        (make-function (cadr exp) (cddr exp) env)
      RAZ_environnement
        (do (reset! env-global env-global-base-value)
            'Rien)
      (let [f-expr (car exp)
            f-val  (evaluate (car exp) env conf)
            args   (evlis (cdr exp) env conf)]
        (do (invoke f-expr f-val args))))))

(defn safe-evaluate
  [exp env conf]
  (try
    (evaluate exp env conf)
    (catch js/Object e e)))

(defn evaluate-str
  ([src env]
   (evaluate-str src env {}))
  ([src env conf]
   (let [result (microalg.parser/parser src)]
     (match result
      [:sexpr sexpr] (safe-evaluate sexpr env conf)
      ; at this point the result should be an error, we forward it
      :else result))))

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
  [exps env conf]
  (if (pair? exps)
    ; this version forces left to right eval
    (let [argument1 (evaluate (car exps) env conf)]
                    (cons argument1 (evlis (cdr exps) env conf))); !!! [] vs ()
    []))

(defn invoke
  [expr fun args]  ; changed `fn` to `fun` and added `expr` for error msg
  (if (fn? fun)
    (try
      (fun args)
      (catch js/Object e
        (match e
          [:incorrect-arity expected-arity arity args]
            (wrong :incorrect-arity expr (str expr) expected-arity arity args)
          :else e)))
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
      ; not like in the book: should be caught in `invoke`
      (throw [:incorrect-arity arity (count args) args]))))

(def env-global-base-value
  {'Rien 'Rien
   'Vrai 'Vrai
   'Faux 'Faux
   'foo 'Rien
   '+ (make-prim "+" + 2)
   })

(def env-global (atom env-global-base-value))

(defn lookup
  [id env]
  (let [value (id @env)]
    (if (nil? value)  ; nil can't be a value in MicroAlg
      (wrong :no-such-binding id (str id)))
      value))

; TODO: lock Rien Vrai Faux and primitives
(defn update!
  [id env value]
  (let [oldvalue (id @env)]
    (if (nil? oldvalue)  ; nil can't be a value in MicroAlg
      (wrong :no-such-binding id (str id))
      (do
        (swap! env assoc id value)
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
      (swap! env #(update % variables (constantly values)))
    (coll? variables)
      (let [num-vars (count variables)
            num-vals (count values)]
        (cond
          (> num-vars num-vals)
            (wrong :too-less-values (first variables))
          (< num-vars num-vals)
            (wrong :too-much-values (first variables))
          :else (swap! env #(apply assoc % (map vector variables values))))
    :else (wrong :cannot-handle-this-env-extension nil nil variables values))))
