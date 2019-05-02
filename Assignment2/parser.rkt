#lang racket
(provide (all-defined-out))

;; =============================================================================
;; Scheme Parser
;;
;; Parse a sexp into a scheme expression according to the following CFG:
;; <exp> ::= <define> | <cexp>              / def-exp | cexp
;; <define> ::= ( define <var> <cexp> )     / def-exp(var:var-exp, val:cexp)
;; <var> ::= <identifier>                   / var-exp(var:symbol)
;; <binding> ::= ( <var> <cexp> )           / binding(var:var-exp, val:cexp)
;; <cexp> ::= <number>                      / num-exp(val:number)
;;         |  <boolean>                     / bool-exp(val:boolean)
;;         |  <string>                      / str-exp(val:string)
;;         |  ( lambda ( <var>* ) <cexp>+ ) / proc-exp(params:List(var-exp), body:List(cexp))
;;         |  ( if <cexp> <cexp> <cexp> )   / if-exp(test: cexp, then: cexp, else: cexp)
;;         |  ( let ( binding* ) <cexp>+ )  / let-exp(bindings:List(binding), body:List(cexp))
;;         |  ( let* ( binding* ) <cexp>+ )  / let*-exp(bindings:List(binding), body:List(cexp))
;;         |  ( <cexp> <cexp>* )            / app-exp(operator:cexp, operands:List(cexp))
;;         |  ( quote <sexp> )              / literal-exp(val:sexp)

;; Define the types
;; All the expression types are disjoint union types.
;; ================
;; For each one: constructor, type-predicate and getters
;; 
;; def-exp: [var:symbol, val:cexp]


; Signature: make-def-exp(var,val)
; Type:[cexp*cexp->cexp]
; Purpose: def-exp value constructor
; Pre-conditions: true
; Tests:(make-def-exp '(var-exp zz) '(num-exp 4))=>(def-exp (var-exp zz) (num-exp 4))
(define make-def-exp (lambda (var val) (list 'def-exp var val)))


; Signature: def-exp?(x)
; Type: [T->boolean]
; Purpose: def-exp? type predicate
; Pre-conditions: true
; Tests:  (def-exp? 5 ) => #f
(define def-exp? (lambda (x) (and (list? x) (eq? (car x) 'def-exp))))

; Signature: def-exp->var(x)
; Type: [def-exp -> cexp]
; Purpose: An accessor for the var of a def-exp expression
; Pre-conditions: none
; Tests:  (def-exp->var (make-def-exp '(var-exp zz) '(num-exp 4)))
(define def-exp->var (lambda (exp) (second exp)))

; Signature: def-exp->val(x)
; Type: [def-exp -> cexp]
; Purpose: An accessor for the var of a def-exp expression
; Pre-conditions: x is def-exp
; Tests:  (def-exp->val (make-def-exp '(var-exp zz) '(num-exp 4)))
(define def-exp->val (lambda (exp) (third exp)))

;; var: [var:symbol]

; Signature: make-var-exp(var,val)
; Type:[T->var-exp]
; Purpose: var-exp value constructor
; Pre-conditions: none
; Tests:  (make-var-exp 2)=>(var-exp 2)
(define make-var-exp (lambda (symbol) (list 'var-exp symbol)))

; Signature:var-exp?(x)
; Type: [T->boolean]
; Purpose: var-exp? type predicate
; Pre-conditions: true
; Tests:  (var-exp? 5 ) => #f
(define var-exp? (lambda (x) (and (list? x) (eq? (car x) 'var-exp))))

; Signature: var-exp->var(x)
; Type: [var-exp -> list(cexp)]
; Purpose: An accessor for the var of a var-exp expression
; Pre-conditions: none
; Tests:  (var-exp->var (make-var-exp '(var-exp zz) ))
(define var-exp->var (lambda (exp) (second exp)))

;; binding: [var:var, val:cexp]

; Signature: make-binding(var,cexp)
; Type:[Any*Any->Any]
; Purpose: binding-exp value constructor
; Pre-conditions: none
; Tests:(make-binding '(var-exp zz) '(num-exp 4))=>(def-exp (var-exp zz) (num-exp 4))
(define make-binding (lambda (var cexp) (list 'binding var cexp)))

; Signature: binding?(x)
; Type: [T->boolean]
; Purpose: binding? type predicate
; Pre-conditions: none
; Tests:  (binding? 'z ) => #f
(define binding? (lambda (x) (and (list? x) (eq? (car x) 'binding))))


; Signature: binding->var(x)
; Type: [binding -> list(cexp)]
; Purpose: An accessor for the var of a binding expression
; Pre-conditions: none
; Tests:  (binding->var '(z 3 2) )=>3  
(define binding->var (lambda (x) (second x)))

; Signature: binding->val(x)
; Type: [binding -> list(cexp)]
; Purpose: An accessor for the val of a binding expression
; Pre-conditions: none
; Tests:  (binding->val '(z 3 2) )=>2
(define binding->val (lambda (x) (third x)))

;; cexp is a disjoint union type

; Signature:cexp?(x) 
; Type: [T ->boolean]
; Purpose:check if x is cexp
; Pre-conditions: none
; Tests: (cexp? (make-var-exp '(var-exp zz)))

(define cexp? (lambda (x)
                (or (num-exp? x)
                    (bool-exp? x)
                    (str-exp? x)
                    (var-exp? x)
                    (literal-exp? x)
                    (proc-exp? x)
                    (if-exp? x)
                    (let-exp? x)
                    (app-exp? x)
                    (let*-exp? x))))

;; num-exp: [val:number]

; Signature: make-num-exp(x)
; Type: [T -> num-exp]
; Purpose: nun-exp value constructor
; Pre-conditions: none
; Tests:  (make-num-exp 3)=>'(num-exp 3) 
(define make-num-exp (lambda (val) (list 'num-exp val)))

; Signature: num-exp?(x)
; Type: [T->boolean]
; Purpose: num-exp? type predicate
; Pre-conditions: none
; Tests:  (num-exp? '(num-exp 3))=>#t
(define num-exp? (lambda (x) (and (list? x) (eq? (car x) 'num-exp))))

;; num-exp: [val:number]

; Signature: num-exp->val(x)
; Type: [num-exp -> number)]
; Purpose: An accessor for the val of a num-exp expression
; Pre-conditions: none
; Tests:(num-exp->val '(num-exp 3) )=>3
(define num-exp->val (lambda (x) (second x)))

; Signature: make-bool-exp(x)
; Type: [T -> bool-exp]
; Purpose: bool-exp value constructor
; Pre-conditions: none
; Tests:  (make-bool-exp #t)=>'(bool-exp #t)
(define make-bool-exp (lambda (val) (list 'bool-exp val)))

; Signature:bool-exp?(x)
; Type:[T->boolean]
; Purpose:  bool-exp? type predicate
; Pre-conditions: none
; Tests:  (bool-exp? (make-bool-exp #t))=>'(bool-exp #t)
(define bool-exp? (lambda (x) (and (list? x) (eq? (car x) 'bool-exp))))

; Signature: bool-exp->val(x)
; Type: [bool-exp -> boolean]
; Purpose: An accessor for the val of a bool-exp expression
; Pre-conditions: none
; Tests:(bool-exp->val '(bool-exp #t) )=>#t
(define bool-exp->val (lambda (x) (second x)))


;; str-exp: [val:string]
; Signature: make-str-exp(x)
; Type: [T -> str-exp]
; Purpose: str-exp value constructor
; Pre-conditions: none
; Tests:  (make-str-exp "orrrr")=>'(str-exp "orrrr")
(define make-str-exp (lambda (val) (list 'str-exp val)))

; Signature:str-exp?(x)
; Type:[T->boolean]
; Purpose: str-exp? type predicate
; Pre-conditions: none
; Tests:  (str-exp? (make-str-exp "#t"))=>#t
(define str-exp? (lambda (x) (and (list? x) (eq? (car x) 'str-exp))))

; Signature: str-exp->val(x)
; Type: [str-exp -> boolean]
; Purpose: An accessor for the val of a str-exp expression
; Pre-conditions: none
; Tests:(str-exp->val '(str-exp "#t") )=>"#t"
(define str-exp->val (lambda (x) (second x)))

;; literal-exp: [val:sexp]
; Signature: make-literal-exp(x)
; Type: [T -> literal-exp]
; Purpose: literal-exp value constructor
; Pre-conditions: none
; Tests:  (make-literal-exp 'zoharit)=>'(literal-exp zoharit)
(define make-literal-exp (lambda (val) (list 'literal-exp val)))

; Signature:literal-exp?(x)
; Type:[T->boolean]
; Purpose:  literal-exp? type predicate
; Pre-conditions: none
; Tests:  (literal-exp? (make-literal-exp 'blat))=>#t
(define literal-exp? (lambda (x) (and (list? x) (eq? (car x) 'literal-exp))))

; Signature: literal-exp->val(x)
; Type: [bool-exp -> cexp]
; Purpose: An accessor for the val of a literal-exp expression
; Pre-conditions: none
; Tests:(literal-exp->val '(literal-exp zooo) )=>'zooo
(define literal-exp->val (lambda (x) (second x)))

; Signature: make-proc-exp(x)
; Type: [T*T -> proc-exp]
; Purpose: proc-exp value constructor
; Pre-conditions: none
; Tests:  (make-proc-exp '(z s d) '(2 3 4))=>'(proc-exp (z s d) (2 3 4))
(define make-proc-exp (lambda (params body) (list 'proc-exp params body)))

; Signature:proc-exp?(x)
; Type:[T->boolean]
; Purpose:  proc-exp? type predicate
; Pre-conditions: none
; Tests:  (proc-exp? (make-proc-exp '(z s d) '(2 3 4))=>#t
(define proc-exp? (lambda (x) (and (list? x) (eq? (car x) 'proc-exp))))

; Signature: proc-exp->params(x)
; Type: [proc-exp -> cexp]
; Purpose: An accessor for the params of a literal-exp expression
; Pre-conditions: none
; Tests:(proc-exp->params (make-proc-exp '(z s d) '(2 3 4)) )=>'(z s d)
(define proc-exp->params (lambda (x) (second x)))


; Signature: proc-exp->body(x)
; Type: [proc-exp -> cexp]
; Purpose: An accessor for the body of a literal-exp expression
; Pre-conditions: none
; Tests:(proc-exp->params (make-proc-body '(z s d) '(2 3 4)) )=>'(2 3 4)
(define proc-exp->body (lambda (x) (third x)))


;; if-exp: [test:cexp, then:cexp, else:cexp]
; Signature: make-if-exp(test then else)
; Type: [boolean*T*T -> if-exp]
; Purpose: if-exp value constructor
; Pre-conditions: none
; Tests:   (make-if-exp(= 2 4) 1 5)=>'(if-exp #f 1 5)
(define make-if-exp (lambda (test then else) (list 'if-exp test then else)))

; Signature:if-exp?(x)
; Type:[T->boolean]
; Purpose: if-exp? type predicate
; Pre-conditions: none
; Tests:  (if-exp? (make-if-exp (= 2 4) 1 5))=>#t
(define if-exp? (lambda (x) (and (list? x) (eq? (car x) 'if-exp))))

; Signature: if-exp->test(x)
; Type: [if-exp -> boolean]
; Purpose: An accessor for the test of a if-exp expression
; Pre-conditions: none
; Tests:(if-exp->test (make-if-exp (= 2 4) 1 5) )=>#f
(define if-exp->test (lambda (x) (second x)))

; Signature: if-exp->then(x)
; Type: [if-exp -> cexp]
; Purpose: An accessor for the then of a if-exp expression
; Pre-conditions: none
; Tests:(if-exp->then (make-if-exp (= 2 4) 1 5) )=>1
(define if-exp->then (lambda (x) (third x)))

; Signature: if-exp->else(x)
; Type: [if-exp -> cexp]
; Purpose: An accessor for the else of a if-exp expression
; Pre-conditions: none
; Tests:(if-exp->else (make-if-exp (= 2 4) 1 5) )=>5
(define if-exp->else (lambda (x) (fourth x)))

;; let-exp: [bindings:List(binding), body:List(cexp)]
; Signature: make-let-exp(bindings body)
; Type: [T*T -> let-exp]
; Purpose: let-exp value constructor
; Pre-conditions: none
; Tests:   (make-let-exp '(2 4) '(1 5))=>'(let-exp (2 4) (1 5))
(define make-let-exp (lambda (bindings body) (list 'let-exp bindings body)))

; Signature:let-exp?(x)
; Type:[T->boolean]
; Purpose: let-exp? type predicate
; Pre-conditions: none
; Tests:  (let-exp? (make-let-exp '( 2 4) '(1 5)))=>#t
(define let-exp? (lambda (x) (and (list? x) (eq? (car x) 'let-exp))))

; Signature: let-exp->bindings(x)
; Type: [let-exp -> cexp]
; Purpose: An accessor for the bindings of a let-exp expression
; Pre-conditions: none
; Tests:(let-exp->bindings (make-let-exp '(2 4) '(1 5)) )=>'(2 4)
(define let-exp->bindings (lambda (x) (second x)))

; Signature: let-exp->body(x)
; Type: [let-exp -> cexp]
; Purpose: An accessor for the body of a let-exp expression
; Pre-conditions: none
; Tests:(let-exp->body (make-let-exp '(2 4) '(1 5)) )=>'(1 5)
(define let-exp->body (lambda (x) (third x)))

;; app-exp: [rator:cexp, rands:List(cexp)]

; Signature: make-app-exp(bindings body)
; Type: [T*T -> app-exp]
; Purpose: app-exp value constructor
; Pre-conditions: none
; Tests:   (make-app-exp '(2 4) '(1 5))=>'(app-exp (2 4) (1 5))
(define make-app-exp (lambda (rator rands) (list 'app-exp rator rands)))

; Signature:app-exp?(x)
; Type:[T->boolean]
; Purpose: app-exp? type predicate
; Pre-conditions: none
; Tests:  (app-exp? (make-app-exp '( 2 4) '(1 5)))=>#t
(define app-exp? (lambda (x) (and (list? x) (eq? (car x) 'app-exp))))

; Signature: app-exp->rator(x)
; Type: [app-exp -> cexp]
; Purpose: An accessor for the rator of a app-exp expression
; Pre-conditions: none
; Tests:(app-exp->rator (make-app-exp '(2 4) '(1 5)) )=>'(2 4)
(define app-exp->rator (lambda (x) (second x)) )

; Signature: app-exp->rands(x)
; Type: [app-exp -> cexp]
; Purpose: An accessor for the rands of a app-exp expression
; Pre-conditions: none
; Tests:(app-exp->rands (make-app-exp '(2 4) '(1 5)) )=>'(1 5)
(define app-exp->rands (lambda (x) (third x)))



; Signature: make-let*-exp(bindings,body)
; Type:[bindings:List(bindings)*body:List(cexp)->let*-exp]
; Purpose: let* value constructor
; Pre-conditions: none
; Tests: (make-let*-exp '(2 4) '(1 5))=>'(let*-exp (2 4) (1 5))
(define make-let*-exp (lambda (bindings body) (list 'let* bindings body)))


; Signature: let*-exp?(x)
; Type: [T->boolean]
; Purpose: let* type predicate
; Pre-conditions: none
; Tests: ( let*-exp?(make-let*-exp '(2 4) '(1 5)))=>#t
(define let*-exp? (lambda (x) (and (list? x) (eq? (car x) 'let*))))

; Signature: let*-exp->bindings(x)
; Type: [let*-exp -> list(pair(var, cexp))]
; Purpose: An accessor for the bindings of a let* expression
; Pre-conditions: none
; Tests: ( let*-exp?->bindings(make-let*-exp '(2 4) '(1 5)))=>'(2 4)
(define let*-exp->bindings (lambda (x) (second x)))

; Signature: let*-exp->body(x)
; Type:[let*-exp -> list(cexp)]
; Purpose: An accessor for the body of a let* expression
; Pre-conditions: none
; Tests: ( let*-exp?->body(make-let*-exp '(2 4) '(1 5)))=>'(1 5)
(define let*-exp->body (lambda (x) (third x)))


; Signature: parse(sexp)
; Type: [Sexp -> Exp]
; Purpose: parse a sexp into a Scheme abstract syntax tree (AST) expression.
; Pre-conditions: none
; Tests: (parse '(define v 1)) -> '(def-exp (var-exp v) (num-exp 1))

;; Examples:
;; (parse '1) -> '(num-exp 1)
;; (parse '(define v 1)) -> '(def-exp (var-exp v) (num-exp 1))
;; (parse '(if #t (+ 1 2) 'ok)) -> '(if-exp (bool-exp #t) (app-exp (var-exp +) ((num-exp 1) (num-exp 2))) (literal-exp ok))
(define parse
  (lambda (sexp)
    (cond ((empty? sexp) (error "unexpected " sexp))
          ((and (list? sexp)
                (eq? (first sexp) 'define))
           (if (and (= (length sexp) 3)
                    (symbol? (second sexp)))
               (make-def-exp (make-var-exp (second sexp))
                             (parse-cexp (third sexp)))
               (error "bad define expression " exp)))
          (else (parse-cexp sexp)))))
;; Signature: parse-cexp(sexp)
;; Type: [Sexp -> Cexp]
;; Purpose: parse a sexp into a cexp (an expression which is not define)
; Pre-conditions: none
; Tests: (parse '(define v 1)) -> '(def-exp (var-exp v) (num-exp 1))


(define parse-cexp
  (lambda (sexp)
    (cond ((number? sexp) (make-num-exp sexp))
          ((boolean? sexp) (make-bool-exp sexp))
          ((string? sexp) (make-str-exp sexp))
          ((symbol? sexp) (make-var-exp sexp))
          ((empty? sexp) (error "unexpected empty"))
          (else (let ((first (car sexp)))
                  (cond ((eq? first 'quote)
                         (make-literal-exp (second sexp)))
                        ((eq? first 'lambda)
                         (make-proc-exp (map make-var-exp (second sexp))
                                        (map parse-cexp (cddr sexp))))
                        ((eq? first 'if)
                         (make-if-exp (parse-cexp (second sexp))
                                      (parse-cexp (third sexp))
                                      (parse-cexp (fourth sexp))))
                        ((eq? first 'let)
                         (make-let-exp (map (lambda (pair)
                                              (make-binding (make-var-exp (car pair))
                                                            (parse-cexp (second pair))))
                                            (second sexp))
                                       (map parse-cexp (cddr sexp))))
                       ((eq? first 'let*)
                         (make-let*-exp (map (lambda (pair)
                                              (make-binding (make-var-exp (car pair))
                                                            (parse-cexp (second pair))))
                                            (second sexp))
                                       (map parse-cexp (cddr sexp))))
                        (else ;; app (rator . rands)
                         (make-app-exp (parse-cexp (car sexp))
                                       (map parse-cexp (cdr sexp))))
                         ))))
            ))


;; =============================================================================
;; Unparse
; Signature: unparse(exp)
; Type: [Exp -> Sexp]
; Purpose: Map an abstract syntax tree to a concrete syntax sexp.
; Pre-conditions: none
; Tests: (unparse (def-exp (var-exp v) (num-exp 1))->'(define v 1)) 

;; Example: (unparse (parse '(lambda (x) x))) => '(lambda (x) x)
(define unparse
  (lambda (exp)
    (cond ((def-exp? exp) (list 'define
                                (unparse (def-exp->var exp))
                                (unparse (def-exp->val exp))))
          ((cexp? exp)
           (cond ((num-exp? exp)  (num-exp->val exp))
                 ((bool-exp? exp) (bool-exp->val exp))
                 ((str-exp? exp)  (str-exp->val exp))
                 ((var-exp? exp)  (var-exp->var exp))
                 ((literal-exp? exp) (list 'quote (literal-exp->val exp)))
                 ((proc-exp? exp) (cons 'lambda
                                        (cons (map unparse (proc-exp->params exp))
                                              (map unparse (proc-exp->body exp)))))
                 ((if-exp? exp) (list 'if
                                      (unparse (if-exp->test exp))
                                      (unparse (if-exp->then exp))
                                      (unparse (if-exp->else exp))))
                 ((let-exp? exp) (cons 'let
                                       (cons (map (lambda (b) (list (unparse (binding->var b))
                                                                    (unparse (binding->val b))))
                                                  (let-exp->bindings exp))
                                             (map unparse (let-exp->body exp)))))
                 ((app-exp? exp) (cons (unparse (app-exp->rator exp))
                                       (map unparse (app-exp->rands exp))))
                 (else (error "Unknown exp type: " exp))))
          (else (error "Unknown exp type: " exp)))))
