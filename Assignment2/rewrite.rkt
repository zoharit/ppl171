#lang racket
(require "parser.rkt")

(provide (all-defined-out))

;; <cexp> ::= <number>                      / num-exp(val:number)
;;         |  <boolean>                     / bool-exp(val:boolean)
;;         |  <string>                      / str-exp(val:string)
;;         |  <variable>                    / var-exp(var:symbol)
;;         |  ( lambda ( <var>* ) <cexp>+ ) / proc-exp(params:List(var-exp), body:List(cexp))
;;         |  ( if <cexp> <cexp> <cexp> )   / if-exp(test: cexp, then: cexp, else: cexp)
;;         |  ( let ( binding* ) <cexp>+ )  / let-exp(bindings:List(binding), body:List(cexp))
;;         |  ( <cexp> <cexp>* )            / app-exp(rator:cexp, rands:List(cexp))
;;         |  ( quote <sexp> )              / literal-exp(val:sexp)
;;         |  ( let* ( binding* ) <cexp>+ )  / let*-exp(bindings:List(binding), body:List(cexp))
;Signature: rewrite-let(cexp)
;Type: [Cexp -> Cexp]
;Purpose: rewrite a single let-exp as a lambda-application form
;Pre-conditions:none
;Tests: (unparse (rewrite-let (parse '(let ((x 1)) (+ x x))))) => '((lambda (x) (+ x x)) 1)

; Signature: unparse(exp)
; Type: [Exp -> Sexp]
; Purpose: Map an abstract syntax tree to a concrete syntax sexp.
; Pre-conditions: none
; Tests: (unparse (def-exp (var-exp v) (num-exp 1))->'(define v 1)) 

(define rewrite-let
  (lambda (cexp)
    (let ((vars (map binding->var (let-exp->bindings cexp)))
          (vals (map binding->val (let-exp->bindings cexp)))
          (body (let-exp->body cexp)))
      (make-app-exp 
       (make-proc-exp vars body)
       vals))))

;Signature: rewrite-all-let(exp)
;Type: [Exp -> Exp]
;Purpose: rewrite all occurrences of let in an expression to lambda-applications.
;Pre-conditions:none
;Tests: (unparse (rewrite-all-let (parse '(lambda (x) (let ((y (+ x 2))) (* y y)))))) => '(lambda (x) ((lambda (y) (* y y)) (+ x 2)))

(define rewrite-all-let
  (lambda (exp)
    (cond ((def-exp? exp) (make-def-exp (def-exp->var exp)
                                        (rewrite-all-let (def-exp->val exp))))
          ((num-exp? exp) exp)
          ((bool-exp? exp) exp)
          ((str-exp? exp)  exp)
          ((var-exp? exp)  exp)
          ((literal-exp? exp) exp)
          ((proc-exp? exp) (make-proc-exp (proc-exp->params exp)
                                          (map rewrite-all-let (proc-exp->body exp))))
          ((if-exp? exp) (make-if-exp
                          (rewrite-all-let (if-exp->test exp))
                          (rewrite-all-let (if-exp->then exp))
                          (rewrite-all-let (if-exp->else exp))))
          ((let-exp? exp) (rewrite-all-let (rewrite-let exp)))
          ((app-exp? exp) (make-app-exp (rewrite-all-let (app-exp->rator exp))
                                        (map rewrite-all-let (app-exp->rands exp))))
          (else (error "Unknown exp type: " exp)))))

;Signature: rewrite-let*(cexp)
;Type: [Cexp -> Cexp]
;Purpose: rewrite a single let*-exp as a lambda-application form
;Pre-conditions:none
;Tests:(unparse (rewrite-let* (parse '(let* ((x 1)) (+ x x))))) => '((lambda (x) (+ x x)) 1)
 
; Signature: rewrite-all-let*(exp)
; Type: [CExp -> CExp]
; Purpose: rewrite expressions containing let*-exp into ASTs that contain only let-exp
;Pre-conditions:none
;Tests:(rewrite-all-let* (parse '(let* ((x 5) (y (f x))) (+ x y) (- y x))))=>'(let-exp ((binding (var-exp x) (num-exp 5)))(let-exp((binding (var-exp y) (app-exp (var-exp f) ((var-exp x)))))((app-exp (var-exp +) ((var-exp x) (var-exp y))) (app-exp (var-exp -) ((var-exp y) (var-exp x))))))
(define rewrite-all-let*
  (lambda (exp)
    (cond ((def-exp? exp) (make-def-exp (def-exp->var exp)
                                        (rewrite-all-let*(def-exp->val exp))))
          ((num-exp? exp) exp)
          ((bool-exp? exp) exp)
          ((str-exp? exp)  exp)
          ((var-exp? exp)  exp)
          ((literal-exp? exp) exp)
          ((proc-exp? exp) (make-proc-exp (proc-exp->params exp)
                                          (map rewrite-all-let* (proc-exp->body exp))))
          ((if-exp? exp) (make-if-exp
                          (rewrite-all-let* (if-exp->test exp))
                          (rewrite-all-let* (if-exp->then exp))
                          (rewrite-all-let* (if-exp->else exp))))
                    ((let-exp? exp)
           (let ((vars (map binding->var (let-exp->bindings exp)))
                 (vals (map binding->val (let-exp->bindings exp)))
                 (body (let-exp->body exp)))  
             (make-let-exp (map make-binding
                                vars
                                (map rewrite-all-let* vals))
                           (map rewrite-all-let* body))))

          ((let*-exp? exp) (let ((vars (map binding->var (let*-exp->bindings exp)))
          (vals  (map binding->val (let*-exp->bindings exp)))
          (body (let*-exp->body exp)))
      (cond ((empty? vars) (make-let-exp '()  (map rewrite-all-let* body)))                      
      ((empty? (cdr vars))
          (make-let-exp (list (make-binding (car vars) (rewrite-all-let* (car vals)))) (map rewrite-all-let* body))
             )(else (make-let-exp (list (make-binding (car vars) (rewrite-all-let* (car vals))))
                                     (list (rewrite-all-let* (make-let*-exp (cdr ( let*-exp->bindings exp)) body)))))
   )))
 
          ((app-exp? exp) (make-app-exp (rewrite-all-let* (app-exp->rator exp))
                                        (map rewrite-all-let* (app-exp->rands exp))))
          (else (error "Unknown exp type: " exp)))))

