#lang racket
(require "utils.rkt"
         "env-closure.rkt"
         "env-box.rkt"
         "env-ast.rkt"
         "env-box-eval.rkt")

(provide (all-defined-out))

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define env-box-eval-tests
  (lambda ()
    (display "env-box-eval-tests:\t")
    (let ((ge the-global-env))
      (run-tests
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) 1) (/ 1 0))) ge) => '1)
       (test (env-box-eval (parseL4 '(+ 1 2)) ge) => '3)
       (test (env-box-eval (parseL4 '((lambda ((x lazy) (b lazy) c) c) (/ 1 0) (/ 1 0) 1)) ge) => '1)
       (test (env-box-eval (parseL4 '((lambda ((x lazy) y) (if ( = y 0) y x)) (/ 1 0) 0)) ge) => '0)
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) ((lambda ((x lazy)) 0) (/ 1 0))) (/ 1 0))) ge) => '0)
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) 1) (/ 1 0))) ge) => '1) ; basic test
       (test (env-box-eval-program (parseL4 '(L4 (define factorial (lambda (fact) ; check that we didn't break existing code
                                                                     (if ( = 0 fact) 1 (* fact (factorial (- fact 1)))))) (factorial 5)))) => '120)
       (test (env-box-eval (parseL4 '((lambda ((x lazy)) x) 5)) ge) => '5) ; check that we evaluate var-refs
       
       ; missing test for exception
       (test (env-box-eval-program (parseL4 '(L4
                                      (define f (lambda (x) (+ x x)))
                                      (f 2)
                                      ))) => '4)
       (test (try (lambda () (env-box-eval-program (parseL4 '(L4
                                              (define f 
                                                (lambda (a (b lazy))
                                                  a))
                                              (f (/ 1 0) 1)
                                      ))))) => 'error)
       (test (try (lambda () (env-box-eval-program (parseL4 '(L4
                                              ((lambda (a b (c lazy) d) (/ a d))
                                               (/ 1 0) 2 3 4)
                                      ))))) => 'error)
       (test (try (lambda () (env-box-eval-program (parseL4 '(L4
                                                              (define f 
                                                                (lambda (a (b lazy))
                                                                  a))
     
                                                              (f 1 (/ 1 0))
                                      ))))) => 1)
       (test (try (lambda () (env-box-eval-program (parseL4 '(L4
                                                              (define q
                                                                (lambda ((a lazy) b)
                                                                  b))
                                                              (define f 
                                                                (lambda (a (b lazy))
                                                                  (q b a)))
     
                                                              (f 5 (/ 1 0))
                                      ))))) => 5)

       (test (try (lambda () (env-box-eval-program (parseL4 '(L4
                                                               (define bla
                                                                (lambda (a b c d (e lazy))
                                                                  (+ a b c d)))
                                                              (bla 1 2 3 4 (1 / 0)) 
                                      ))))) => 10)      
       ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(env-box-eval-tests)

;;;;;;;;;;;;;;;;;;;;;;;;;; not my tests below ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define env-box-eval-tests2
  (lambda ()
    (display "env-box-eval-tests:\t")
    (let ((ge the-global-env))
      (run-tests
       (test (env-box-eval (parseL4 '1) ge) => 1)
       (test (env-box-eval (parseL4 '#t) ge) => #t)
       (test (env-box-eval (parseL4 'x) (extend-env-box ge '(x) '(1))) => 1)
       (test (env-box-eval (parseL4 '+) ge) => '(prim-op +))
       (test (env-box-eval (parseL4 '(+ 1 2)) ge) => 3)
       (test (env-box-eval (parseL4 '(> 2 1)) ge) => #t)
       (test (env-box-eval (parseL4 '(not (> 2 1))) ge) => #f)
       (test (env-box-eval (parseL4 '(+ (* 2 2) 3)) ge) => 7)

       ;; L2 syntactic forms
       (test (env-box-eval (parseL4 '(if (> 2 1) 3 -3)) ge) => 3)
       (test (env-box-eval (parseL4 '(lambda (x) x)) ge) => (make-closure '((var-decl x)) '((var-ref x)) ge))

       ;; L4 syntactic forms
       (test (env-box-eval (parseL4 '(cons 1 '())) ge) => '(1))
       (test (env-box-eval (parseL4 '(car '(1 . 2))) ge) => 1)
       (test (env-box-eval (parseL4 '(cdr '(1 2))) ge) => '(2))
       (test (env-box-eval (parseL4 '(number? 'x)) ge) => #f)
       (test (env-box-eval (parseL4 '(symbol? 'x)) ge) => #t)
       (test (env-box-eval (parseL4 '(list? (cons 1 2))) ge) => #f)
       (test (env-box-eval (parseL4 '(pair? (cons 1 2))) ge) => #t)
       (test (env-box-eval (parseL4 '(boolean? #t)) ge) => #t)
       (test (env-box-eval (parseL4 '(eq? 'x 'x)) ge) => #t)

       ))))

(define env-box-eval-program-tests
  (lambda ()
    (display "env-box-eval-program-tests:\t")
    (run-tests
     (test (env-box-eval-program (parseL4 '(L4 (define x (+ 3 2))
                                               (* x x)))) => 25)
     (test (env-box-eval-program (parseL4 '(L4 (define x 1)))) => (void))
     (test (env-box-eval-program (parseL4 '(L4 (define x 3) (* x x) (+ x x)))) => 6)
     (test (env-box-eval-program (parseL4 '(L4 (define x 3) (not (> x 2))))) => #f)

     ;; Procedure application
     (test (env-box-eval-program (parseL4 '(L4 (define f (lambda (x) (* x x))) (f 3)))) => 9)
     (test (env-box-eval-program (parseL4 '(L4 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3)))) => 3)
     
     ;; Accidental capture of the z variable if no renaming
     (test (env-box-eval-program
            (parseL4
             '(L4
               (define z (lambda (x) (* x x)))
               (((lambda (x) (lambda (z) (x z)))
                 (lambda (w) (z w)))
                2))))
           =>
           4)

     ;; Y-combinator
     (test (env-box-eval-program
            (parseL4
             '(L4 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1)))))))
                   6))))
           =>
           720)

     (test (env-box-eval-program
            (parseL4
             '(L4 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                  ((compose not number?) 2))))
           =>
           #f)

     )))

(define env-box-eval-program-closure-tests
  (lambda ()
    (display "env-box-eval-program-closure-tests:\t")
    (run-tests
     ;; make closures
     (test (env-box-eval-program
            (parseL4
             '(L4 (define make-pair
                    (lambda (a b)
                      (lambda (msg)
                        (if (eq? msg 'car)
                            a
                            (if (eq? msg 'cdr)
                                b
                                -1)))))
                  (let ((p (make-pair 'a 'b)))
                    (p 'cdr)))))
           =>
           'b)
     )))

(define env-box-eval-program-letrec-tests
  (lambda ()
    (display "env-box-eval-program-letrec-tests:\t")
    (run-tests
     ;; letrec
     (test (env-box-eval-program
            (parseL4
             '(L4 (define f
                    (lambda (x)
                      (letrec ((fact (lambda (x)
                                       (if (= x 1)
                                           1
                                           (* x (fact (- x 1)))))))
                        (fact x))))
                  (f 5))))
           =>
           120)

     ;; mutual recursion
     (test (env-box-eval-program
            (parseL4
             '(L4 (define f
                    (lambda (x)
                      (letrec ((even? (lambda (x)
                                        (if (= x 0)
                                            #t
                                            (odd? (- x 1)))))
                               (odd? (lambda (x)
                                       (if (= x 0)
                                           #f
                                           (even? (- x 1))))))
                        (even? x))))
                  (f 10))))
           =>
           #t)

     ;; letrec with non-procedural values
     (test (env-box-eval-program
            (parseL4
             '(L4 (letrec ((x 1) (y 2)) (+ x y)))))
           =>
           3)
     )))

(define env-box-eval-program-recursion-tests
  (lambda ()
    (display "env-box-eval-program-recursion-tests:\t")
    (run-tests
     ;; Recursive procedure
     (test (env-box-eval-program (parseL4 '(L4 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3)))) => 6)

     ;; Preserve bound variables in subst
     (test (env-box-eval-program
            (parseL4
             '(L4 (define nf
                    (lambda (f n)
                      (if (= n 0)
                          (lambda (x) x)
                          (if (= n 1)
                              f
                              (lambda (x) (f ((nf f (- n 1)) x)))))))
                  ((nf (lambda (x) (* x x)) 2) 3)))) => 81)

     ;; L4 higher order functions
     (test (env-box-eval-program
            (parseL4
             '(L4 (define map
                    (lambda (f l)
                      (if (eq? l '())
                          l
                          (cons (f (car l)) (map f (cdr l))))))
                  (map (lambda (x) (* x x))
                       '(1 2 3)))))
           =>
           '(1 4 9))
     
     (test (env-box-eval-program
            (parseL4
             '(L4 (define empty? (lambda (x) (eq? x '())))
                  (define filter
                    (lambda (pred l)
                      (if (empty? l)
                          l
                          (if (pred (car l))
                              (cons (car l) (filter pred (cdr l)))
                              (filter pred (cdr l))))))
                  (filter (lambda (x) (not (= x 2)))
                          '(1 2 3 2)))))
           =>
           '(1 3))
     )))

(define env-box-eval-program-global-tests
  (lambda ()
    (display "env-box-eval-program-global-tests:\t")
    (run-tests
     ;; Global environment effect
     (test (env-box-eval-program
            (parseL4
             '(L4 (define g (lambda (x) 5))
                  (define f (lambda (x) (g x)))
                  (f 2))))
           =>
           5)

     ;; We obtain the Scheme effect back
     (test (env-box-eval-program
            (parseL4
             '(L4 (define f (lambda (x) (g x)))
                  (define g (lambda (x) 5))
                  (f 2))))
           =>
           5)

     ;; Mutually recursive top-level procedures
     (test (env-box-eval-program
            (parseL4
             '(L4 (define even? (lambda (x) (if (= 0 x) #t (odd? (- x 1)))))
                  (define odd? (lambda (x) (if (= 0 x) #f (even? (- x 1)))))
                  (odd? 9))))
           =>
           #t)
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(env-box-eval-tests2)
(env-box-eval-program-tests)
(env-box-eval-program-closure-tests)
(env-box-eval-program-letrec-tests)
(env-box-eval-program-recursion-tests)
(env-box-eval-program-global-tests)

(define normal-L4-tests
  (lambda ()
    (display "normal-L4-tests:\t")
    (let ((ge the-global-env))
      (run-tests
       (test (env-box-eval (parseL4 '1) ge) => 1)
       (test (env-box-eval (parseL4 '#t) ge) => #t)
       (test (env-box-eval (parseL4 '+) ge) => '(prim-op +))
       (test (env-box-eval (parseL4 '(+ 1 2)) ge) => 3)
       (test (env-box-eval (parseL4 '(> 2 1)) ge) => #t)
       (test (env-box-eval (parseL4 '(not (> 2 1))) ge) => #f)
       (test (env-box-eval (parseL4 '(+ (* 2 2) 3)) ge) => 7)

       ;; L2 syntactic forms
       (test (env-box-eval (parseL4 '(if (> 2 1) 3 -3)) ge) => 3)

       ;; L4 syntactic forms
       (test (env-box-eval (parseL4 '(cons 1 '())) ge) => '(1))
       (test (env-box-eval (parseL4 '(car '(1 . 2))) ge) => 1)
       (test (env-box-eval (parseL4 '(cdr '(1 2))) ge) => '(2))
       (test (env-box-eval (parseL4 '(number? 'x)) ge) => #f)
       (test (env-box-eval (parseL4 '(symbol? 'x)) ge) => #t)
       (test (env-box-eval (parseL4 '(list? (cons 1 2))) ge) => #f)
       (test (env-box-eval (parseL4 '(pair? (cons 1 2))) ge) => #t)
       (test (env-box-eval (parseL4 '(boolean? #t)) ge) => #t)
       (test (env-box-eval (parseL4 '(eq? 'x 'x)) ge) => #t)

       ))))

(define normal-program-L4-tests
  (lambda ()
    (display "normal-program-L4-tests:\t")
    (run-tests
     (test (env-box-eval-program (parseL4 '(L4 (define x (+ 3 2))
                                            (* x x)))) => 25)
     (test (env-box-eval-program (parseL4 '(L4 (define x 1)))) => (void))
     (test (env-box-eval-program (parseL4 '(L4 (define x 3) (* x x) (+ x x)))) => 6)
     (test (env-box-eval-program (parseL4 '(L4 (define x 3) (not (> x 2))))) => #f)

     ;; Procedure application
     (test (env-box-eval-program (parseL4 '(L4 (define f (lambda (x) (* x x))) (f 3)))) => 9)
     (test (env-box-eval-program (parseL4 '(L4 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3)))) => 3)

     ;; Recursive procedure
     (test (env-box-eval-program (parseL4 '(L4 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3)))) => 6)

     ;; Preserve bound variables in subst
     (test (env-box-eval-program
            (parseL4
             '(L4 (define nf
                    (lambda (f n)
                      (if (= n 0)
                          (lambda ((x lazy)) x)
                          (if (= n 1)
                              f
                              (lambda (x) (f ((nf f (- n 1)) x)))))))
                  ((nf (lambda (x) (* x x)) 2) 3)))) => 81)

     ;; Accidental capture of the z variable if no renaming
     (test (env-box-eval-program
            (parseL4
             '(L4
               (define z (lambda ((x lazy)) (* x x)))
               (((lambda (x) (lambda (z) (x z)))
                 (lambda (w) (z w)))
                2))))
           =>
           4)

     ;; Y-combinator
     (test (env-box-eval-program
            (parseL4
             '(L4 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda ((n lazy))
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1)))))))
                   6))))
           =>
           720)

     ;; L4 higher order functions
     (test (env-box-eval-program
            (parseL4
             '(L4 (define map
                    (lambda ((f lazy) (l lazy))
                      (if (eq? l '())
                          l
                          (cons (f (car l)) (map f (cdr l))))))
                  (map (lambda (x) (* x x))
                       '(1 2 3)))))
           =>
           '(1 4 9))

     (test (env-box-eval-program
            (parseL4
             '(L4 (define empty? (lambda ((x lazy)) (eq? x '())))
                  (define filter
                    (lambda (pred l)
                      (if (empty? l)
                          l
                          (if (pred (car l))
                              (cons (car l) (filter pred (cdr l)))
                              (filter pred (cdr l))))))
                  (filter (lambda ((x lazy)) (not (= x 2)))
                          '(1 2 3 2)))))
           =>
           '(1 3))

     (test (env-box-eval-program
            (parseL4
             '(L4 (define compose (lambda ((f lazy) g) (lambda (x) (f (g x)))))
                  ((compose not number?) 2))))
           =>
           #f)

     )))

(define normal-only-L4-tests
  (lambda ()
    (display "normal-only-L4-tests:\t")
    (run-tests
     (test (env-box-eval-program
            (parseL4
             ;; This program loops in eval-order - but completes in normal order
             '(L4 (define loop (lambda () (loop)))
                  (define f (lambda (x y z) (if (= x 1) y z)))
                  (f 1 2 (loop)))))
           =>
           2)

     (test (env-box-eval-program
            (parseL4
             '(L4
               (define loop (lambda ((x lazy)) (loop x)))
               (define g (lambda (x) 5))
               (g (loop 0)))))
           =>
           5)

     (test (env-box-eval-program
            (parseL4
             '(L4 
               (define try 
                 (lambda ((a lazy) (b lazy)) 
                   (if (= a 0)
                       1
                       b)))
               (try 0 (/ 1 0)))))
           =>
           1)

     (test (env-box-eval-program
            (parseL4
             '(L4
               (define f (lambda ((x lazy)) (display x) (newline) (+ x 1)))
               (define g (lambda ((x lazy)) 5))
               (g (f 0)))))
           =>
           5)
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(normal-L4-tests)
(normal-program-L4-tests)