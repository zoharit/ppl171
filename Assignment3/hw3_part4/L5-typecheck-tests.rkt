#lang racket
(require "utils.rkt"
         "tenv.rkt"
         "L5-ast.rkt"
         "L5-typecheck.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define parse-texp-tests
  (lambda ()
    (display "parse-texp-tests:\t")
    (run-tests
     (test (parse-texp 'number) => 'num-te)
     (test (parse-texp 'boolean) => 'bool-te)
     (test (parse-texp 'T1) => '(tvar T1 #&#f))
     (test (parse-texp '(T * T -> boolean)) => '(proc-te ((tvar T #&#f) (tvar T #&#f)) bool-te))
     (test (parse-texp '(number -> (number -> number))) => '(proc-te (num-te) (proc-te (num-te) num-te)))
     (test (parse-texp 'void) => 'void-te)
     (test (parse-texp '(Empty -> void)) => '(proc-te () void-te))
     )))

(define unparse-texp-tests
  (lambda ()
    (display "unparse-texp-tests:\t")
    (run-tests
     (test (unparse-texp 'num-te) => 'number)
     (test (unparse-texp 'bool-te) => 'boolean)
     (test (unparse-texp '(tvar T1 #&#f)) => 'T1)
     (test (unparse-texp '(proc-te ((tvar T #&#f) (tvar T #&#f)) bool-te)) => '(T * T -> boolean))
     (test (unparse-texp '(proc-te (num-te) (proc-te (num-te) num-te))) => '(number -> (number -> number)))
     )))

(define parseL5-tests
  (lambda ()
    (display "parseL5-tests:\t")
    (run-tests
     (test (parseL5 '(define [x : number] 1)) => '(def-exp (var-decl x num-te) (num-exp 1)))
     (test (parseL5 '(lambda ([x : number]) : number x))
           =>
           '(proc-exp ((var-decl x num-te)) ((var-ref x)) num-te))
     )))

(define myAstTests
  (lambda ()
    (display "parseL5-tests:\t")
    (run-tests
     (test (unparse-texp (make-pair-te 'num-te 'num-te)) => '(Pair number number))
     (test (unparse-texp (make-pair-te (make-pair-te 'num-te 'bool-te) 'num-te)) => '(Pair (Pair number boolean) number))
     (test (unparse-texp (make-pair-te (make-pair-te 'num-te 'bool-te) (make-pair-te 'bool-te 'num-te))) => '(Pair (Pair number boolean) (Pair boolean number))) 
)))


(define myDefineTests (lambda ()
                        (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))
                         
                          (run-tests
                          (test (do '(define (f : [number -> number]) (lambda ([n : number]) : number (* n n)))) => 'void)
                          (test (do '(define (f : [number * number -> number]) (lambda ([n : number] [m : number]) : number (* m n)))) => 'void)
                          (test (do '(define (f : [number * number -> number]) (lambda ([n : number] [m : number]) : number (* (+ 1 m) ((lambda ([x : number]) : number x) n))))) => 'void)
                          (test (do '(define (f : [[number -> number] -> number]) (lambda ([g : [number -> number]]) : number (g 3)))) => 'void)
                          #;(test (do '(define (f : [number -> number]) (lambda ([n : boolean]) : number (* n n)))) => 'void)
                          )                      
                        )
                        )
  )

(define myAppPairTests (lambda ()
                        (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))                         
                          (run-tests
                          (test (do '(lambda ([a : number] [b : number]): (Pair number number)
                              (cons a b))) => '(number * number -> (Pair number number)))
                          (test (do '(lambda ([a : number] [b : number]): (Pair number number)
                              (cons 1 2))) => '(number * number -> (Pair number number)))
                          (test (do '(lambda ([a : boolean] [b : number]): (Pair number number)
                              (cons 1 2))) => '(boolean * number -> (Pair number number)))
                          (test (do '(lambda ([a : boolean] [b : number]): (Pair boolean number)
                               (cons a 2))) => '(boolean * number -> (Pair boolean number)))                        
                          (test (do '(lambda ([a : boolean] [b : number]): (Pair boolean number)
                               (cons a 2))) => '(boolean * number -> (Pair boolean number)))
                          (test (do '(lambda ([a : (Pair boolean boolean)]) : (Pair boolean boolean)
                               a)) => '((Pair boolean boolean) -> (Pair boolean boolean)))
                          (test (do '(lambda ([a : (Pair boolean boolean)]) : (Pair boolean boolean)
                               (cons 1 2)
                               (cons #t #f)
                               )) => '((Pair boolean boolean) -> (Pair boolean boolean)))      
                          ))))

(define myLetPairTests (lambda ()
                        (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))                         
                          (run-tests
                          (test (do '(let (([x : (Pair number number)] (cons 1 2))) x)) => '(Pair number number))
                          (test (do '(let (([x : (Pair number number)] (cons 1 2))) (car x))) => 'number)
                          (test (do '(lambda ([x : (Pair number number)]) : (Pair number number)
                              (let (([y : (Pair number number)] x)) y))) => '((Pair number number) -> (Pair number number)))
                          (test (do '(letrec (([p1 : ((Pair boolean number) -> number)] (lambda ([x : (Pair boolean number)]) : number 2)))
                              p1)) => '((Pair boolean number) -> number))
                           (test (do '(letrec (([p1 : ((Pair boolean number) -> number)] (lambda ([x : (Pair boolean number)]) : number (cdr x))))
                              p1)) => '((Pair boolean number) -> number))
))))

(define myAppCarCdrTests (lambda ()
                        (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))                         
                          (run-tests
                          (test (do '(lambda ([a : (Pair number number)]): number
                              (car a))) => '((Pair number number) -> number))
                          (test (do '(lambda ([a : (Pair number number)]): number
                              (cdr a))) => '((Pair number number) -> number))
                          (test (do '(lambda ([a : (Pair boolean number)]): number
                              (cdr a))) => '((Pair boolean number) -> number))
                          (test (do '(lambda ([a : (Pair boolean (Pair boolean (Pair number boolean)))]): number
                              (car (cdr (cdr a))))) => '((Pair boolean (Pair boolean (Pair number boolean))) -> number))                          
))))

(define myLetCarCdrTests (lambda ()
                        (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))                         
                          (run-tests
                          (test (do '(let (([x : number] (car (cons 1 2)))) #f)) => 'boolean)
                          (test (do '(let (([x : boolean] (cdr (cons 1 #f)))) x)) => 'boolean)
                          (test (do '(let (([x : number] (cdr (cons #f 2)))) x)) => 'number)
                          (test (do '(lambda ([x : (Pair number boolean)]) : (Pair boolean number)
                              (let (([y : boolean] (cdr x))) (cons y 1)))) => '((Pair number boolean) -> (Pair boolean number)))
                          (test (do '(lambda ([x : (Pair number (Pair boolean number))]) : (Pair boolean (Pair number boolean))
                              (let (([y : boolean] (car (cdr x)))) (cons y (cons 1 y))))) => '((Pair number (Pair boolean number)) -> (Pair boolean (Pair number boolean)))) 
))))


(define typeof-exp-tests
  (lambda ()
    (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))
      (display "typeof-exp-tests:\t")
      (run-tests
       (test (do 5) => 'number)
       (test (do #t) => 'boolean)
       
       (test (do '+) => '(number * number -> number))
       (test (do '-) => '(number * number -> number))
       (test (do '*) => '(number * number -> number))
       (test (do '/) => '(number * number -> number))
       (test (do '=) => '(number * number -> boolean))
       (test (do '<) => '(number * number -> boolean))
       (test (do '>) => '(number * number -> boolean))
       (test (do 'not) => '(boolean -> boolean))

       (test (typeof-exp (parseL5 'x) (extend-tenv (make-empty-tenv) '(x) '(num-te))) => 'num-te)

       (test (do '(if (> 1 2) 1 2)) => 'number)
       (test (do '(if (= 1 2) #t #f)) => 'boolean)

       (test (do '(lambda ([x : number]) : number x)) => '(number -> number))
       (test (do '(lambda ([x : number]) : boolean (> x 1))) => '(number -> boolean))

       (test (do '(lambda ([x : number]) : (number -> number) (lambda ([y : number]) : number (* y x))))
             =>
             '(number -> (number -> number)))

       (test (do '(lambda ([f : (number -> number)]) : number (f 2)))
             =>
             '((number -> number) -> number))

       (test (do '(let (([x : number] 1)) (* x 2))) => 'number)
       
       (test (do '(let (([x : number] 1)
                        ([y : number] 2))
                    (lambda ([a : number]) : number (+ (* x a) y))))
             =>
             '(number -> number))

       (test (do '(lambda ([x : number]) : number
                    (let (([y : number] x)) (+ x y))))
             =>
             '(number -> number))

       (test (do '(letrec (([p1 : (number -> number)] (lambda ([x : number]) : number (* x x))))
                    p1))
             => '(number -> number))

       (test (do '(letrec (([p1 : (number -> number)] (lambda ([x : number]) : number (* x x))))
                    (p1 2)))
             => 'number)

       (test (do '(letrec (([odd? : (number -> boolean)] (lambda ([n : number]) : boolean
                                                           (if (= n 0) #f (even? (- n 1)))))
                           ([even? : (number -> boolean)] (lambda ([n : number]) : boolean
                                                            (if (= n 0) #t (odd? (- n 1))))))
                    (odd? 12)))
             => 'boolean)
       ))))
       
(define typeof-exp-tests-polymorphic
  (lambda ()
    (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))
      (display "typeof-exp-tests-polymorphic:\t")
      (run-tests
       (test (do '(lambda ([x : T1]) : T1 x)) => '(T1 -> T1))

       (test (do '(let (([x : number] 1))
                    (lambda ([y : T] [z : T]) : T (if (> x 2) y z))))
             =>
             '(T * T -> T))

       (test (do '(lambda () : number 1)) => '(Empty -> number))
       
       ))))

(define myQuoteTests (lambda ()
                        (letrec ((do (lambda (exp) (unparse-texp (typeof-exp (parseL5 exp) (make-empty-tenv))))))
                          (display "myQuoteTests:\t")
                          (run-tests
                          (test (do '(quote 5)) => 'number)
                          (test (do '(quote x)) => 'symbol)
                          (test (do '(quote #t)) => 'boolean)
                          (test (do '(quote #f)) => 'boolean)
                          (test (do '(quote (1 . 2))) => '(Pair number number))
                          (test (do '(quote (#t . 2))) => '(Pair boolean number))
                          (test (do '(quote (#t . #t))) => '(Pair boolean boolean))
                          (test (do '(quote (2 . #t))) => '(Pair number boolean))
                          (test (do '(quote (x . #t))) => '(Pair symbol boolean))
                          (test (do '(quote (x . adsdas))) => '(Pair symbol symbol))
                          (test (do '(quote sadsadasds)) => 'symbol)
                          (test (do '(quote 3212121d)) => 'symbol)
                          (test (do '(lambda ([x : symbol]) : number (quote 5)) ) => '[symbol -> number])
                          (test (do '(lambda ([x : symbol]) : boolean (quote #t)) ) => '[symbol -> boolean])
                          (test (do '(lambda ([x : symbol]) : symbol (quote x)) ) => '[symbol -> symbol])
                          (test (do '(define [a : [symbol -> symbol]] (lambda ([x : symbol]) : symbol (quote x))))
                                => 'void)
))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(myAstTests) ; 3 tests
(myDefineTests) ;4 tests
(myAppPairTests) ;7 tests
(myLetPairTests)  ;5 tests
(myAppCarCdrTests) ;4 tests
(myLetCarCdrTests) ;5 tests
;28 tests overall

(myQuoteTests)

(parse-texp-tests)
(unparse-texp-tests)
(parseL5-tests)
(typeof-exp-tests)
(typeof-exp-tests-polymorphic)