#lang racket
(require "utils.rkt"
         "env.rkt"
         "L1-ast.rkt"
         "L2-ast.rkt"
         "L3-ast.rkt"
         "L2-eval.rkt"
         "L3-eval.rkt")

;; Support exception handling
(define (try f (ans 'error))
  (with-handlers ((exn?
                   (lambda (exn) ans))) (f)))

(define tests
  (lambda ()
    (display "tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L3-applicative-eval (parseL3 '((lambda ((a lazy)) 1) (/ 1 0))) ge) => '1)
       (test (L3-applicative-eval (parseL3 '(+ 1 2)) ge) => '3)
        (test (L3-applicative-eval (parseL3 '((lambda (y) (y (/ 1 0))) (lambda ((x lazy)) 1))) ge) => '1)
        (test (L3-applicative-eval (parseL3 '((lambda ((x lazy) y) (y x)) (/ 1 0) (lambda ((x lazy)) 1))) ge) => '1)
        ;(test (L3-applicative-eval (parseL3 '((lambda (x) (x (/ 1 0))) (lambda ((x lazy)) ((lambda ((x lazy)) 1) (/ 1 0))))) ge) => '1)

       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) 1) (/ 1 0))) ge) => '1)
       (test (L3-applicative-eval (parseL3 '(+ 1 2)) ge) => '3)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy) (b lazy) c) c) (/ 1 0) (/ 1 0) 1)) ge) => '1)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy) y) (if ( = y 0) y x)) (/ 1 0) 0)) ge) => '0)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) ((lambda ((x lazy)) 0) (/ 1 0))) (/ 1 0))) ge) => '0)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) 1) (/ 1 0))) ge) => '1) ; basic test
       (test (eval-L3-program (parseL3 '(L3 (define factorial (lambda (fact) ; check that we didn't break existing code
                                                                     (if ( = 0 fact) 1 (* fact (factorial (- fact 1)))))) (factorial 5)))) => '120)
       (test (L3-applicative-eval (parseL3 '((lambda ((x lazy)) x) 5)) ge) => '5) ; check that we evaluate var-refs
        
       (test (eval-L3-program (parseL3 '(L3
                                      (define f (lambda (x) (+ x x)))
                                      (f 2)
                                      ))) => '4)
       (test (try (lambda () (eval-L3-program (parseL3 '(L3
                                              (define f 
                                                (lambda (a (b lazy))
                                                  a))
                                              (f (/ 1 0) 1)
                                      ))))) => 'error)
       (test (try (lambda () (eval-L3-program (parseL3 '(L3
                                              ((lambda (a b (c lazy) d) (/ a d))
                                               (/ 1 0) 2 3 4)
                                      ))))) => 'error)
       (test (try (lambda () (eval-L3-program (parseL3 '(L3
                                                              (define f 
                                                                (lambda (a (b lazy))
                                                                  a))
     
                                                              (f 1 (/ 1 0))
                                      ))))) => 1)
       (test (try (lambda () (eval-L3-program (parseL3 '(L3
                                                              (define q
                                                                (lambda ((a lazy) b)
                                                                  b))
                                                              (define f 
                                                                (lambda (a (b lazy))
                                                                  (q b a)))
     
                                                              (f 5 (/ 1 0))
                                      ))))) => 5)

       (test (try (lambda () (eval-L3-program (parseL3 '(L3
                                                               (define bla
                                                                (lambda (a b c d (e lazy))
                                                                  (+ a b c d)))
                                                              (bla 1 2 3 4 (1 / 0)) 
                                      ))))) => 10)         
       ))))

(tests)


;;;;;;;;;;;;;;;;;;;;;;; not my tests below: ;;;;;;;;;;;;;;;;;;;;;;;;;;


(define eval-L3-tests
  (lambda ()
    (display "eval-L3-tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L3-applicative-eval (parseL3 '1) ge) => 1)
       (test (L3-applicative-eval (parseL3 '#t) ge) => #t)
       (test (L3-applicative-eval (parseL3 'x) (extend-env ge '(x) '(1))) => 1)
       (test (L3-applicative-eval (parseL3 '+) ge) => '(prim-op +))
       (test (L3-applicative-eval (parseL3 '(+ 1 2)) ge) => 3)
       (test (L3-applicative-eval (parseL3 '(> 2 1)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(not (> 2 1))) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(+ (* 2 2) 3)) ge) => 7)

       ;; L2 syntactic forms
       (test (L3-applicative-eval (parseL3 '(if (> 2 1) 3 -3)) ge) => 3)
       (test (L3-applicative-eval (parseL3 '(lambda (x) x)) ge) => '(closure ((var-decl x)) ((var-ref x))))

       ;; L3 syntactic forms
       (test (L3-applicative-eval (parseL3 '(cons 1 '())) ge) => '(1))
       (test (L3-applicative-eval (parseL3 '(car '(1 . 2))) ge) => 1)
       (test (L3-applicative-eval (parseL3 '(cdr '(1 2))) ge) => '(2))
       (test (L3-applicative-eval (parseL3 '(number? 'x)) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(symbol? 'x)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(list? (cons 1 2))) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(pair? (cons 1 2))) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(boolean? #t)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(eq? 'x 'x)) ge) => #t)

       ))))

(define eval-program-L3-tests
  (lambda ()
    (display "eval-program-L3-tests:\t")
    (run-tests
     (test (eval-L3-program (parseL3 '(L3 (define x (+ 3 2))
                                       (* x x)))) => 25)
     (test (eval-L3-program (parseL3 '(L3 (define x 1)))) => (void))
     (test (eval-L3-program (parseL3 '(L3 (define x 3) (* x x) (+ x x)))) => 6)
     (test (eval-L3-program (parseL3 '(L3 (define x 3) (not (> x 2))))) => #f)

     ;; Procedure application
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (* x x))) (f 3)))) => 9)
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3)))) => 3)

     ;; Recursive procedure
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3)))) => 6)

     ;; Preserve bound variables in subst
     (test (eval-L3-program
            (parseL3
             '(L3 (define nf
                    (lambda (f n)
                      (if (= n 0)
                          (lambda (x) x)
                          (if (= n 1)
                              f
                              (lambda (x) (f ((nf f (- n 1)) x)))))))
                  ((nf (lambda (x) (* x x)) 2) 3)))) => 81)

     ;; Accidental capture of the z variable if no renaming
     (test (eval-L3-program
            (parseL3
             '(L3
               (define z (lambda (x) (* x x)))
               (((lambda (x) (lambda (z) (x z)))
                 (lambda (w) (z w)))
                2))))
           =>
           4)

     ;; Y-combinator
     (test (eval-L3-program
            (parseL3
             '(L3 (((lambda (f) (f f))
                    (lambda (fact)
                      (lambda (n)
                        (if (= n 0)
                            1
                            (* n ((fact fact) (- n 1)))))))
                   6))))
           =>
           720)

     ;; L3 higher order functions
     (test (eval-L3-program
            (parseL3
             '(L3 (define map
                    (lambda (f l)
                      (if (eq? l '())
                          l
                          (cons (f (car l)) (map f (cdr l))))))
                  (map (lambda (x) (* x x))
                       '(1 2 3)))))
           =>
           '(1 4 9))

     (test (eval-L3-program
            (parseL3
             '(L3 (define empty? (lambda (x) (eq? x '())))
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

     (test (eval-L3-program
            (parseL3
             '(L3 (define compose (lambda (f g) (lambda (x) (f (g x)))))
                  ((compose not number?) 2))))
           =>
           #f)

     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-L3-tests)
(eval-program-L3-tests)

(define normal-L3-tests
  (lambda ()
    (display "normal-L3-tests:\t")
    (let ((ge (make-empty-env)))
      (run-tests
       (test (L3-applicative-eval (parseL3 '1) ge) => 1)
       (test (L3-applicative-eval (parseL3 '#t) ge) => #t)
       (test (L3-applicative-eval (parseL3 'x) (extend-env ge '(x) '(1))) => 1)
       (test (L3-applicative-eval (parseL3 '+) ge) => '(prim-op +))
       (test (L3-applicative-eval (parseL3 '(+ 1 2)) ge) => 3)
       (test (L3-applicative-eval (parseL3 '(> 2 1)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(not (> 2 1))) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(+ (* 2 2) 3)) ge) => 7)

       ;; L2 syntactic forms
       (test (L3-applicative-eval (parseL3 '(if (> 2 1) 3 -3)) ge) => 3)
       (test (L3-applicative-eval (parseL3 '(lambda ((x lazy)) x)) ge) => '(closure ((var-decl (lazy x))) ((var-ref x))))

       ;; L3 syntactic forms
       (test (L3-applicative-eval (parseL3 '(cons 1 '())) ge) => '(1))
       (test (L3-applicative-eval (parseL3 '(car '(1 . 2))) ge) => 1)
       (test (L3-applicative-eval (parseL3 '(cdr '(1 2))) ge) => '(2))
       (test (L3-applicative-eval (parseL3 '(number? 'x)) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(symbol? 'x)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(list? (cons 1 2))) ge) => #f)
       (test (L3-applicative-eval (parseL3 '(pair? (cons 1 2))) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(boolean? #t)) ge) => #t)
       (test (L3-applicative-eval (parseL3 '(eq? 'x 'x)) ge) => #t)

       ))))

(define normal-program-L3-tests
  (lambda ()
    (display "normal-program-L3-tests:\t")
    (run-tests
     (test (eval-L3-program (parseL3 '(L3 (define x (+ 3 2))
                                            (* x x)))) => 25)
     (test (eval-L3-program (parseL3 '(L3 (define x 1)))) => (void))
     (test (eval-L3-program (parseL3 '(L3 (define x 3) (* x x) (+ x x)))) => 6)
     (test (eval-L3-program (parseL3 '(L3 (define x 3) (not (> x 2))))) => #f)

     ;; Procedure application
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (* x x))) (f 3)))) => 9)
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (if (> x 0) x (- 0 x)))) (f -3)))) => 3)

     ;; Recursive procedure
     (test (eval-L3-program (parseL3 '(L3 (define f (lambda (x) (if (= x 0) 1 (* x (f (- x 1)))))) (f 3)))) => 6)

     ;; Preserve bound variables in subst
     (test (eval-L3-program
            (parseL3
             '(L3 (define nf
                    (lambda (f n)
                      (if (= n 0)
                          (lambda ((x lazy)) x)
                          (if (= n 1)
                              f
                              (lambda (x) (f ((nf f (- n 1)) x)))))))
                  ((nf (lambda (x) (* x x)) 2) 3)))) => 81)

     ;; Accidental capture of the z variable if no renaming
     (test (eval-L3-program
            (parseL3
             '(L3
               (define z (lambda ((x lazy)) (* x x)))
               (((lambda (x) (lambda (z) (x z)))
                 (lambda (w) (z w)))
                2))))
           =>
           4)

  
     ;; L3 higher order functions
     (test (eval-L3-program
            (parseL3
             '(L3 (define map
                    (lambda ((f lazy) (l lazy))
                      (if (eq? l '())
                          l
                          (cons (f (car l)) (map f (cdr l))))))
                  (map (lambda (x) (* x x))
                       '(1 2 3)))))
           =>
           '(1 4 9))

     (test (eval-L3-program
            (parseL3
             '(L3 (define empty? (lambda ((x lazy)) (eq? x '())))
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

     (test (eval-L3-program
            (parseL3
             '(L3 (define compose (lambda ((f lazy) g) (lambda (x) (f (g x)))))
                  ((compose not number?) 2))))
           =>
           #f)

     )))

(define normal-only-L3-tests
  (lambda ()
    (display "normal-only-L3-tests:\t")
    (run-tests
     (test (eval-L3-program
            (parseL3
             ;; This program loops in eval-order - but completes in normal order
             '(L3 (define loop (lambda () (loop)))
                  (define f (lambda (x y z) (if (= x 1) y z)))
                  (f 1 2 (loop)))))
           =>
           2)

     (test (eval-L3-program
            (parseL3
             '(L3
               (define loop (lambda ((x lazy)) (loop x)))
               (define g (lambda (x) 5))
               (g (loop 0)))))
           =>
           5)

     (test (eval-L3-program
            (parseL3
             '(L3 
               (define try 
                 (lambda ((a lazy) (b lazy)) 
                   (if (= a 0)
                       1
                       b)))
               (try 0 (/ 1 0)))))
           =>
           1)

     (test (eval-L3-program
            (parseL3
             '(L3
               (define f (lambda ((x lazy)) (display x) (newline) (+ x 1)))
               (define g (lambda ((x lazy)) 5))
               (g (f 0)))))
           =>
           5)
     )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Invoke
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(normal-L3-tests)
(normal-program-L3-tests)
  