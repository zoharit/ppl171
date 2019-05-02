#lang racket
(require "parser.rkt")
(provide (all-defined-out))

; Signature: sliding-window(list,window) Type: [List * Number -> List]
; Purpose: Produce a list of lists of length window each with the elements slided by factor window 
;Pre-conditions: none
;Tests: (sliding-window '( 1 2 3 4) 2)=>'((1 2) (2 3) (3 4))
(define sliding-window
  (lambda (list window)
    (if (< (length list) window)
       '()
    (cons (take list window) (sliding-window (cdr list) window)
            ))))

; Signature: greatest-node(tree)
; Type: [(List(Number)union Number) -> Number]
; Purpose: Compute the greatest node data value in the tree
;Pre-conditions:none
;Tests:(greatest-node '( 2 4 5 7))=>7

(define greatest-node
  (lambda (tree)
   (cond ((pair? tree) (max (greatest-node (car tree)) (greatest-node (cdr tree))))
        ((number? tree) tree)
        (else -1))))

; Signature: count-node(tree,x)
; Type: [(List union T1) * T2 -> Number]
; Purpose: Compute the number of nodes whose data value is equal to x
;Pre-conditions:none
;Tests:(count-node '(hi (8) 0) 8)=>1
(define count-node 
  (lambda (tree x)  
        (cond
      ((and (empty? tree) (not(equal? x '()))) 0)
      ((and (empty? tree) (empty? x)) 1) 
      ((not (list? tree))
       (if (equal? tree x) 1 0))
      (else (+ (count-node (car tree) x)
                (count-node (cdr tree) x))))))
                                        

; Signature: mirror-tree(tree)
; Type: [(List union T) -> List]
; Purpose: Compute the mirrored tree
;Pre-conditions:none
;Tests:(mirror-tree '(5 (#t 4 2) (12 () 3)))=>'(5 (12 3 ()) (#t 2 4))
(define mirror-tree
  (lambda (tree)  
    (cond((empty? tree) '())  
         ((not(list? tree)) tree) (else (list (car tree) (if (pair? (caddr tree)) (mirror-tree (caddr tree)) (caddr tree))
                                             (if (pair? (cadr tree)) (mirror-tree(cadr tree)) (cadr tree)))
         )))) 




; Signature: unparse->js(ast,output-port)
; Type: [Exp * Output-Port -> Void]
; Purpose: unparse a Scheme AST into Javascript syntax without considering infix notation
;Pre-conditions:none
;Tests:(unparse->js (parse '(define x (f x (* 5 x) (= 3 2) y)))(current-output-port))->const x = f(x,*(5,x),==(3,2),y);
(define unparse->js
  (lambda (ast output-port)
    (cond ((def-exp? ast) (fprintf output-port "const " ) (unparse->js 
        (def-exp->var ast) output-port)(fprintf output-port " = ") (unparse->js (def-exp->val ast) output-port)(fprintf output-port ";" ))
          ((cexp? ast)
           (cond((num-exp? ast)(fprintf output-port "~a" (num-exp->val ast)))
                ((bool-exp? ast)(fprintf output-port "~a" (if (eq? (bool-exp->val ast) #t) "true" "false")))
                ((str-exp? ast)(fprintf output-port "~s" (str-exp->val ast)))
                ((var-exp? ast)(if (equal?  (cadr ast) '=)(fprintf output-port "==" )  (fprintf output-port "~a"  (var-exp->var  ast ))))
             ((proc-exp? ast) 
                                        (fprintf output-port "(" )
                                            (let ((x (last (proc-exp->params ast))))

                                        (map (lambda (b)
                                             (unparse->js b output-port)
                                          (if (not(eq? x b))
                                                     (fprintf output-port "," ) (fprintf output-port "" ))
                                            )(proc-exp->params ast) ) ) 
                                                   
                                       (fprintf output-port ")" )
                                       (fprintf output-port " => " )
                                        (fprintf output-port "{ " )
                                        (let(( y(last (proc-exp->body ast))))
                                      (map (lambda (b)
                                             (unparse->js b output-port)
                                             (if (not(eq? y b)) 
                                             (fprintf output-port "; " )(fprintf output-port "" )))          
                                           (proc-exp->body ast)))
                                       (fprintf output-port " }" )
                                       
                                     )
              ((let-exp? ast) (fprintf output-port "let ")

                                      
                                      (let ((y (last (let-exp->bindings ast) ))(x (first (let-exp->bindings ast))))

                                      (map (lambda (b)
                                               (if (not(eq? b x))
                                                      (fprintf output-port ", ")
                                                      (fprintf output-port ""))
                                                    
                                                      (unparse->js (binding->var b) output-port)
                                                      (fprintf output-port " = ")
                                                      (unparse->js (binding->val b) output-port)
                                             (cond ((eq? b y)
                                             (fprintf output-port "; ")
                                             ))
                                                      )
                                           (let-exp->bindings ast)
                                       ))
                                     
                                      (map (lambda (b)
                                            
                                             (unparse->js b output-port)
                                              (fprintf output-port ";")
                                             )
                                           (let-exp->body ast))

                              )
              

                          ((if-exp? ast) (list (unparse->js (if-exp->test ast) output-port)
                              (fprintf output-port " ? " )
                              (unparse->js (if-exp->then ast) output-port)
                               (fprintf output-port " : " )
                              (unparse->js (if-exp->else ast) output-port)))
                          ((app-exp? ast)
                                      (unparse->js (app-exp->rator ast) output-port)
                                                  (fprintf output-port "(" )
                                            (let ((x (last (app-exp->rands ast))))
                                              (map (lambda (b)
                                                (unparse->js b output-port)
                                               (if (not(eq? x b))
                                                     (fprintf output-port "," ) (fprintf output-port "" ))
                                                                                          )
                                         (app-exp->rands ast)
                                        ))

                                            (fprintf output-port ")" )


         
                              ) 
             (else (error "Unknown exp type: " ast))))
      (else (error "Unknown exp type: " ast)))
  )
)



; Signature: unparse->js-infix(ast,output-port)
; Type: [Exp * Output-Port -> Void]
; Purpose: unparse a Scheme AST into Javascript syntax while considering infix notation
;Pre-conditions:none
;Tests:(unparse->js-infix (parse '(lambda (x y)(if (= x y)(+ y y)(* (- x 0) y (/ x 1 y)))))(current-output-port))->(x,y)=>{(x == y) ? (y + y) : ((x - 0) * y * (x / 1 / y))}
(define unparse->js-infix
  (lambda (ast output-port)
    (cond ((def-exp? ast) (fprintf output-port "const " ) (unparse->js-infix 
        (def-exp->var ast) output-port)(fprintf output-port "=") (unparse->js-infix (def-exp->val ast) output-port)(fprintf output-port ";" ))
          ((cexp? ast)
           (cond((num-exp? ast)(fprintf output-port "~a" (num-exp->val ast)))
                ((bool-exp? ast)(fprintf output-port "~a" (if (eq? (bool-exp->val ast) #t) " true" " false")))
                ((str-exp? ast)(fprintf output-port "~s" (str-exp->val ast)))
                ((var-exp? ast)  
                 (let ((tmp (var-exp->var  ast) ))
                 (if(not(or (eq? tmp '=)(eq? tmp '+) (eq? tmp '-) (eq? '/ tmp) (eq? '* tmp)))  
                    (fprintf output-port "~a"  (var-exp->var  ast )) (fprintf output-port ""))))
             ((proc-exp? ast)   
                                        (fprintf output-port "(" )
                                            (let ((x (last (proc-exp->params ast))))

                                        (map (lambda (b)
                                             (unparse->js-infix b output-port)
                                          (if (not(eq? x b))
                                                     (fprintf output-port "," ) (fprintf output-port "" ))
                                            )(proc-exp->params ast) ) ) 
                                                   
                                       (fprintf output-port ")" )
                                       (fprintf output-port " => " )
                                        (fprintf output-port "{ " )
                                        (let(( y(last (proc-exp->body ast))))
                                      (map (lambda (b)
                                             (unparse->js-infix b output-port)
                                             (if (not(eq? y b)) 
                                             (fprintf output-port "; " )(fprintf output-port "" )))          
                                           (proc-exp->body ast)))
                                       (fprintf output-port " }" )
                                       
                                     )
              ((let-exp? ast) (fprintf output-port "let ")

                                      
                                      (let ((y (last (let-exp->bindings ast) ))(x (first (let-exp->bindings ast))))

                                      (map (lambda (b)
                                               (if (not(eq? b x))
                                                      (fprintf output-port ",")
                                                      (fprintf output-port ""))
                                                    
                                                      (unparse->js-infix (binding->var b) output-port)
                                                      (fprintf output-port " = ")
                                                      (unparse->js-infix (binding->val b) output-port)
                                             (cond ((eq? b y)
                                             (fprintf output-port "; ")
                                             ))
                                                      )
                                           (let-exp->bindings ast)
                                       ))
                                     
                                      (map (lambda (b)
                                            
                                             (unparse->js-infix b output-port)
                                              (fprintf output-port "; ")
                                             )
                                           (let-exp->body ast))

                              )
              

                          ((if-exp? ast) (list (unparse->js-infix (if-exp->test ast) output-port)
                              (fprintf output-port " ? " )
                              (unparse->js-infix (if-exp->then ast) output-port)
                               (fprintf output-port " : " )
                              (unparse->js-infix (if-exp->else ast) output-port)))
                          ((app-exp? ast)
                                      (unparse->js-infix (app-exp->rator ast) output-port)
                                                  (fprintf output-port "(" )
                                            (let ((x (last (app-exp->rands ast))) (y (cadr(app-exp->rator ast))))
                                              (map (lambda (b)
                                                   
                                                (unparse->js-infix b output-port)
                                               (if (not(eq? x b))
                                                   (cond((eq? y '-)(fprintf output-port " - " ))
                                                        ((eq? y '+)(fprintf output-port " + " ))
                                                        ((eq? y '*)(fprintf output-port " * " ))
                                                        ((eq? y '/)(fprintf output-port " / " ))
                                                        ((eq? y '=)(fprintf output-port " == " ))
                                                     (else(fprintf output-port "," ))) (fprintf output-port "" ))
                                                                                          )
                                         (app-exp->rands ast)
                                        ))

                                            (fprintf output-port ")" )


         
                              ) 
             (else (error "Unknown exp type: " ast))))
      (else (error "Unknown exp type: " ast)))
  )
)


