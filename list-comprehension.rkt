#lang racket
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


 
(define-syntax lc
  (syntax-rules (: <- @)
    [(lc expr : var <- drawn-from) (map (lambda (var) expr) drawn-from)]
    [(lc expr : @ guard) (if guard (list expr) `())]
    [(lc expr : @ guard  qualifier ...) 
     (append* (lc (lc expr : qualifier ...) : @ guard))]
    [(lc expr : var <- drawn-from  qualifier ...) 
     (append* (lc (lc expr :  qualifier ... ) : var <- drawn-from))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; (struct node(t1 t2) #:transparent)
; (lc (+ x y) : x <- '(1 2 3 4 5 6) @(even? x)  y <- '( 7 8 9))
; (lc (node x y) : x <- '(t1 t2 t3)
;                  y <- '(s1 s2))
; (define (qsort l)
;   (cond [(null? l) '()]
;         [else (let ((lows (lc x : x <- (cdr l) @(<= x (car l))))
;                     (highs (lc x : x <- (cdr l) @(> x (car l)))))
;                 (append (qsort lows) (list (car l))
;                         (qsort highs)))]))


