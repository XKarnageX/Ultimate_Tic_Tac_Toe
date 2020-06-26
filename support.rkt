#lang racket
(require 2htdp/image
         2htdp/universe)

(provide (all-defined-out))

(define g (list (list #f #f #f #f #f #f #f #f #f)
           (list #f #f #f #f #f #f #f #f #f)
           (list #f #f #f #f #f #f #f #f #f)
           (list #f #f #f #f #f #f #f #f #f)
           (list #f #f #f #f #f #f #f #f #f)
           (list #f #f #f #f #f #f #f #f #f)
           (list #f #f #f #f #f #f #f #f #f)
           (list #f #f #f #f #f #f #f #f #f)
           (list #f #f #f #f #f #f #f #f #f)))

;;X=1, O=-1 Nothing = #f

(define gt (list #f #f #f #f #f #f #f #f #f))


(define (lposgen pos blist) (list-ref blist (+ (- (* 3 (cdr pos)) 4) (car pos))))
(define (lposget pos) (+ (- (* 3 (cdr pos)) 4) (car pos)))

(define (winning-state b)  ;; returns 1,-1 or #f
  (cond [(equal-rows b) (equal-rows b)] 
        [(equal-columns b) (equal-columns b)]
        [(equal-diagonals b) (equal-diagonals b)]
        [else #f]))

(define (equal-rows b)
  (cond [(myequal? (lposgen (cons 1 1) b) (lposgen (cons 2 1) b) (lposgen (cons 3 1) b)) (lposgen (cons 1 1) b)]
        [(myequal? (lposgen (cons 1 2) b) (lposgen (cons 2 2) b) (lposgen (cons 3 2) b)) (lposgen (cons 1 2) b)]
        [(myequal? (lposgen (cons 1 3) b) (lposgen (cons 2 3) b) (lposgen (cons 3 3) b)) (lposgen (cons 1 3) b)]
        [else #f]))

(define (myequal? a b c)
  (if (equal? #f (and a b c)) #f 
  (and (equal? a b) (equal? b c))))

(define (equal-columns b)
  (cond [(myequal? (lposgen (cons 1 1) b) (lposgen (cons 1 2) b) (lposgen (cons 1 3) b)) (lposgen (cons 1 1) b)]
        [(myequal? (lposgen (cons 2 1) b) (lposgen (cons 2 2) b) (lposgen (cons 2 3) b)) (lposgen (cons 2 1) b)]
        [(myequal? (lposgen (cons 3 1) b) (lposgen (cons 3 2) b) (lposgen (cons 3 3) b)) (lposgen (cons 3 1) b)]
        [else #f]))

(define (equal-diagonals b)
  (cond [(myequal? (lposgen (cons 1 1) b) (lposgen (cons 2 2) b) (lposgen (cons 3 3) b)) (lposgen (cons 1 1) b)]
        [(myequal? (lposgen (cons 3 1) b) (lposgen (cons 2 2) b) (lposgen (cons 1 3) b)) (lposgen (cons 3 1) b)]
        [else #f]))

(define lt (list #f #f #f #f #f #f #f #f #f))



;(define g (m (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)))
;(define gt (m #f #f #f #f #f #f #f #f #f)) ;can be #f,1,0

(define (play-move b t c gt) ;;coordinates=(cons localboardpos eleminlboard) returns void, 1,-1
  (begin
    (define newb (if t
       ; ((car (conv (car c))) b (local-board-gen ((cdr (conv (car c))) b) (cdr c) 1))
        (list-update b (lposget (car c)) (lambda (x) (list-update x (lposget (cdr c)) (lambda (x) 1))))
        ;((car (conv (car c))) b (local-board-gen ((cdr (conv (car c))) b) (cdr c) 0)));;void function t=>X
        (list-update b (lposget (car c)) (lambda (x) (list-update x (lposget (cdr c)) (lambda (x) -1))))))
    (define newgt (if (winning-state (lposgen (car c) newb))
                                      ;((car (conv (car c))) gt (winning-state ((cdr (conv (car c))) b)))
                      (let ([gt1 (list-update gt (lposget (car c)) (lambda (x) (winning-state (lposgen (car c) newb))))])
                                                 gt1) gt))
    (cons newb newgt)))
    
     
;(define (local-board-gen lb y v)
;  (begin ((car (conv y)) lb v) lb))





;(define (reset)
;  (define nb (m (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)
;             (m #f #f #f #f #f #f #f #f #f)))
;
;  (define gtb (m #f #f #f #f #f #f #f #f #f))
;  (cons nb gtb))



         
      