#lang racket
(require 2htdp/image
         2htdp/universe)
(require "list-comprehension.rkt")
(require "support.rkt")
(require "UI.rkt")
(provide (all-defined-out))

;  m-obj is the global board struct
;  gtb is the global board represented as local board
;  db is the image of the current board
;  t represents who is currently playing #t-X, #f-O
;  bound gives which local board the user has to play-on. Eg- (cons 1 3) or (cons #f #f)
;  prev-world returns the previous world

(define state 0)
(define-struct world (m-obj gtb db t bound prev-world))
(define scale-fac 2)
(define sidel (* 300 scale-fac))
(define sqlength (* 20 scale-fac))
(define bigsqlength (* 3 sqlength))
(define es (/ (- sidel (* 3 bigsqlength)) 2)) ;;edit for esx esy? =60

(define s1 (square sqlength 'outline 'blue))
(define r1 (beside s1 s1 s1 s1 s1 s1 s1 s1 s1))
(define gboard0 (above  r1 r1 r1 r1 r1 r1 r1 r1 r1))

(define s2 (square bigsqlength 'outline 'red))
(define r2 (beside s2 s2 s2))
(define gboard1 (above  r2 r2 r2))
(define undotext (text "UNDO" 25 'black))
(define undo (place-image undotext 40 20 (rectangle 80 40 'outline 'black))) 

(define gboardimg
  (place-image (place-image (text "BACK" 25 'black)  40 20
                            (rectangle 80 40 'outline 'black))
               (+ (/ sidel 2) 60) (- sidel (/ gap 2))
               (place-image undo (- (/ sidel 2) 60) (- sidel (/ gap 2))
                            (overlay/offset (overlay gboard1 gboard0) 0 0 (empty-scene sidel sidel)))))

(define circlerad (/ sqlength 4))
(define radialclength (/ sqlength 2))

(define O (circle circlerad "outline" "forestgreen"))
(define X (radial-star 4 0 radialclength "solid" "black"))

(define winning-imageX
  (text "X wins!!!" (* 30 scale-fac) 'black))
(define winlocalimgX
  (rectangle sqlength sqlength 'solid (make-color 10 10 10)))

(define winning-imageO
  (text "O wins!!!" (* 30 scale-fac) 'forestgreen))

(define WORLDINIT1 (make-world (list (list #f #f #f #f #f #f #f #f #f)
                                     (list #f #f #f #f #f #f #f #f #f)
                                     (list #f #f #f #f #f #f #f #f #f)
                                     (list #f #f #f #f #f #f #f #f #f)
                                     (list #f #f #f #f #f #f #f #f #f)
                                     (list #f #f #f #f #f #f #f #f #f)
                                     (list #f #f #f #f #f #f #f #f #f)
                                     (list #f #f #f #f #f #f #f #f #f)
                                     (list #f #f #f #f #f #f #f #f #f))
                               (list #f #f #f #f #f #f #f #f #f)      ; X=1 O=-1
                               mainbackg
                               #t                             ;=X O=#f
                               (cons #f #f)
                               null))

(define WORLDINIT (make-world (list (list #f #f #f #f #f #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f #f)
                                    (list #f #f #f #f #f #f #f #f #f))
                              (list #f #f #f #f #f #f #f #f #f)      ; X=1 O=-1
                              gboardimg
                              #t                             ;=X O=#f
                              (cons #f #f)
                              null))

(define (rand-move w)
  (let* ([all-moves (possible-moves w)]
         [size (length all-moves)]
         [r (if (= size 0) 0 (random size))])
    (if (= size 0) #f 
        (list-ref all-moves r))))


;(if (not (winning-state (lposgen (cons bx by) (car new-board)))) (place-image (XorO (world-t w)) rx ry (world-db w))
;                                                (let ([img (place-image (XorO (world-t w)) rx ry (world-db w))])
;                                                  (if (world-t w) (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 0 255 120)) lbx lby img)
;                                                      (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 255 0 120)) lbx lby img))))

(define (possible-moves w)
;  (displayln (world-db w))
  (if (car (world-bound w))
      (let ([l
      (lc 
       (let ([new-board (play-move (world-m-obj w) (world-t w) (cons (world-bound w) (cons r1 r2)) (world-gtb w))])
         (make-world (car new-board)
                     (cdr new-board)
                     (if (not (winning-state (lposgen (world-bound w) (car new-board))))
                         (place-image (XorO (world-t w))
                                      (convert1 (car (world-bound w))  r1)
                                      (convert1 (cdr (world-bound w))  r2)
                                      (world-db w))
                         (let ([img (place-image (XorO (world-t w))
                                                 (convert1 (car (world-bound w))  r1)
                                                 (convert1 (cdr (world-bound w))  r2)
                                                 (world-db w))]
                               [lbx (- (+ es (* bigsqlength (car (world-bound w)))) (/ bigsqlength 2))]
                               [lby (- (+ es (* bigsqlength (cdr (world-bound w)))) (/ bigsqlength 2))])
                            (if (world-t w) (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 0 255 120)) lbx lby img)
                           (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 255 0 120)) lbx lby img)))) ;;edit rx ry
                     (not (world-t w))
                     (bound-maker r1 r2 (cdr new-board))
                     w
                     )) : r1 <- (list 1 2 3) r2 <- (list 1 2 3)
                        @ (not (filled? (car (world-bound w)) (cdr (world-bound w)) r1 r2 (world-gtb w) (world-m-obj w))))
      ])
        (if (not (null? l)) l (possible-moves (make-world
                                               (world-m-obj w)
                                               (world-gtb w)
                                               (world-db w)
                                               (world-t w)
                                               (cons #f #f)
                                               (world-prev-world w)))))
    ;  (if (> wcount 20)
          (let* ([l (not-won-lb (world-gtb w) null 0)])
                          ;[r4 (car (convertindex (list-ref l (random (length l)))))]
                          ;[r3 (cdr (convertindex (list-ref l (random (length l)))))]) r3 = (cdr (convertindex x))
                          (lc 
                           (let ([new-board (play-move (world-m-obj w) (world-t w) (cons (convertindex x)
                                                                                         (cons r1 r2))
                                                       (world-gtb w))])
                             (make-world (car new-board)
                                         (cdr new-board)
                                         ;(place-image (XorO (world-t w)) (convert1 (cdr (convertindex x))  r1) (convert1 (car (convertindex x))  r2) (world-db w)) 
                                         (if (not (winning-state (lposgen (convertindex x) (car new-board)))) (place-image (XorO (world-t w))
                                                                                                                                                                   (convert1 (car (convertindex x))  r1)
                                                                                                                                                                   (convert1 (cdr (convertindex x))  r2)
                                                                                                                                                                   (world-db w))
                                             (let ([img (place-image (XorO (world-t w))
                                                                     (convert1 (car (convertindex x))  r1)
                                                                     (convert1 (cdr (convertindex x))  r2)
                                                                     (world-db w))]
                                                   [lbx (- (+ es (* bigsqlength (car (convertindex x)))) (/ bigsqlength 2))]
                                                   [lby (- (+ es (* bigsqlength (cdr (convertindex x)))) (/ bigsqlength 2))])
                                                  (if (world-t w) (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 0 255 120)) lbx lby img)
                                               (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 255 0 120)) lbx lby img))))
                                         (not (world-t w))
                                         (bound-maker r1 r2 (cdr new-board))
                                         w
                                         )) : r1 <- (list 1 2 3) r2 <- (list 1 2 3) x <- l
                                            @ (not (filled? (car (convertindex x))
                                                          (cdr (convertindex x))
                                                          r1 r2 (world-gtb w) (world-m-obj w)))))))
          
;          (let ([l (not-won-lb (world-gtb w) null 0)])
;            (if (null? l) l
;                (let* (
;                      [bxby (convertindex (list-ref l (random (length l))))]
;                      [r3 (car bxby)]
;                      [r4 (cdr bxby)])
;                  (lc 
;                   (let ([new-board (play-move (world-m-obj w) (world-t w) (cons (cons r3 r4) (cons r1 r2)) (world-gtb w))])
;                     (make-world (car new-board)
;                                 (cdr new-board)
;                                 ;(place-image (XorO (world-t w)) (convert1 r3 r1) (convert1 r4 r2) (world-db w))
;                                 (if (not (winning-state (lposgen (cons r3 r4) (car new-board)))) (place-image (XorO (world-t w))
;                                                                                                                   (convert1 r3  r1)
;                                                                                                                   (convert1 r4  r2)
;                                                                                                                   (world-db w))
;                                     (let ([img (place-image (XorO (world-t w))
;                                                             (convert1 r3 r1)
;                                                             (convert1 r4 r1)
;                                                             (world-db w))]
;                                           [lbx (- (+ es (* bigsqlength r3)) (/ bigsqlength 2))]
;                                           [lby (- (+ es (* bigsqlength r4)) (/ bigsqlength 2))])
;                                        (if (world-t w) (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 0 255 120)) lbx lby img)
;                                       (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 255 0 120)) lbx lby img))))
;                                 (not (world-t w))
;                                 (bound-maker r1 r2 (cdr new-board))
;                                 w)) : r1 <- (list 1 2 3) r2 <- (list 1 2 3)
;                                     @ (not (filled? r3 r4 r1 r2 (world-gtb w) (world-m-obj w)))))
;
;                ))))





;(if (valid-move (car (world-bound w)) (cdr (world-bound w)) (car (world-bound w)) (cdr (world-bound w)) r1 r2 (world-gtb w) (world-m-obj w))
          
      


(define (convert1 gb lb)
  (+ es (* (- gb 1) bigsqlength) (* sqlength (- lb 0.5))))

(define (run-sim w)
  (if (not (winning-state (world-gtb w)))
      (let ([poss-move (rand-move w)]) (if poss-move (run-sim poss-move) 0)) (winning-state (world-gtb w))))
;...........................................................

;play-random-move : plays a random move, with appropriate player

(struct node (visits wins kids parent world) #:transparent #:mutable) ;kids=list of children nodes
(define root (node 0 0 null null WORLDINIT))
(define treesim 200)

(define N (node-visits root)) ;;parent of root node is null

(define c -0.3)

(define count 5)

(define inf -9999999)

;(define (selector-function x)   ;"A"
;  (if (not (= 0 (node-visits x))) (+ (/ (node-wins x) (node-visits x)) (* c (sqrt (/ (log (node-visits root)) (node-visits x)))))
;            inf))  


;(define (one-random-simulation b)
;  (define (loop) (if (not (winning-state b))(begin
;                                              play-random-move
;                                              (loop))
;                     (winning-state b)))
;  (loop))


(define (back-propagate leaf wins count) (if (null? leaf) (void)    
                                             (begin (set-node-visits! leaf (+ (node-visits leaf) count))
                                                    (set-node-wins! leaf (+ (node-wins leaf) wins))
                                                    (back-propagate (node-parent leaf) wins count))))
  

;"A"
(define (s leaf countloc) ;;simulates count games ;;count is a global variable. It simulates and updates the whole tree.
  (if (not (= countloc 0))
      (begin (set-node-wins! leaf (+ (node-wins leaf) (run-sim (node-world leaf))))
             (s leaf (- countloc 1)))
      (begin
        (set-node-visits! leaf (+ (node-visits leaf) count))
        (back-propagate (node-parent leaf) (node-wins leaf) count))))

;A
 
 
 

;(define (main w)

;A

;A


(define (make-decision w)

  (define (selector-function x)   ;"A"
    (if (not (= 0 (node-visits x))) (+ (/ (node-wins x) (node-visits x)) (* c (sqrt (/ (log (node-visits root)) (node-visits x)))))
        inf))

  (define (selection tree)
    (if (null? (node-kids tree)) (begin (s tree count) (expand tree))
        (selection (argmin selector-function (node-kids tree)))))

  (define (expand leaf)  ;;possible-moves gives a list of nodes which are derived from the current world
    (set-node-kids! leaf (map (lambda (w) (node 0 0 null leaf w)) (possible-moves (node-world leaf)))))

  

  (define (build-tree root)
    (define (loop i) (if (= i 0) root
                         (begin (selection root) (loop (- i 1)))))
    (loop treesim))

  
  (define root (node 0 0 null null w))
  (define dtree (time (build-tree root)))
  (node-world (argmin selector-function (node-kids root))))
  
  



;improve graphics
;add lan-feature
;

;;(expand leaf) "A"
;;(valid-move w) gives a list of leaves "U"
;;change play-move so that it returns a new board instead of set!'ing old board
;;(change world such that it has previous world)
;;comment all functions
;;main driver (build-tree)
  
;.............................................................................................

(define (not-won-lb gt l i)
  (if (null? gt) l
      (if (car gt) (not-won-lb (cdr gt) l (+ i 1))
          (not-won-lb (cdr gt) (cons i l) (+ i 1)))))

(define (convertindex i)
  (match i
    [0 (cons 1 1)]
    [1 (cons 2 1)]
    [2 (cons 3 1)]
    [3 (cons 1 2)]
    [4 (cons 2 2)]
    [5 (cons 3 2)]
    [6 (cons 1 3)]
    [7 (cons 2 3)]
    [8 (cons 3 3)]))

(define (board-maker w)
  (cond [(or (= 1 state) (= 2 state))
         (define (lbgen a)
           (- (+ es (* bigsqlength a)) (/ bigsqlength 2)))
  
         (let* ([dbdb (world-db w)]
                [bound (world-bound w)]
                [img (rectangle bigsqlength bigsqlength 'solid (make-color 255 230 80 50))]
                ) 
           (if (car bound)
               (let* ([lbx (lbgen (car bound))]
                      [lby (lbgen (cdr bound))])
                 (place-image img lbx lby dbdb))
               ;(place-image img1 (+ es (* (/ 3 2) bigsqlength)) (+ es (* (/ 3 2) bigsqlength)) dbdb))))
               (let ([list (not-won-lb (world-gtb w) null 0)])
                 (define (loop list dbdb)
                   (if (null? list) dbdb (place-image img (lbgen (car (convertindex (car list)))) (lbgen (cdr (convertindex (car list)))) (loop (cdr list) dbdb))))
                 (loop list dbdb))))]
        [else (world-db w)]))
        

(define (click w x y action)
  (cond [(and (= state 1) (not (world-t w))) (begin (set! treesim (+ treesim 10)) (make-decision w))]
        [(mouse=? "button-up" action)
         (update w x y)]
        [else w]))

(define button-hwidth 100)
(define button-hheight 40)

(define (button-click x y y-cord)
  (and (<= (abs (- x (/ sidel 2))) button-hwidth)
       (<= (abs (- y y-cord)) button-hheight)))

(define wcount 0)

(define (update w x y) ;;updating current world
  (cond [(= state 0)
         (cond [(button-click x y gap) (begin (set! state 1)
                                              WORLDINIT)]
               [(button-click x y (* 2 gap)) (begin (set! state 2)
                                                    WORLDINIT)]
               [(button-click x y (* 3 gap)) (begin (set! state 3)
                                                    (make-world (list (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f))
                                                                (list #f #f #f #f #f #f #f #f #f)
                                                                howto-backg
                                                                #t
                                                                (cons #f #f)
                                                                null))]
               [(button-click x y (* 4 gap)) (begin (set! state 4)
                                                    (make-world (list (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f)
                                                                      (list #f #f #f #f #f #f #f #f #f))
                                                                (list #f #f #f #f #f #f #f #f #f)
                                                                credits-backg
                                                                #t
                                                                (cons #f #f)
                                                                null))]
               [else w])]
        [(= state 2) (let*([rx (round x)]
                           [ry (round y)]
                           [sx (rounds x)]
                           [sy (rounds y)]
                           [bx (roundb x)]
                           [by (roundb y)]
                           [lbx (- (+ es (* bigsqlength bx)) (/ bigsqlength 2))]
                           [lby (- (+ es (* bigsqlength by)) (/ bigsqlength 2))])
    
    
                       (cond [(and (not (or (out-of-bound x) (out-of-bound y)))
                                   (valid-move (car (world-bound w))
                                               (cdr (world-bound w)) bx by sx sy (world-gtb w) (world-m-obj w)))
                              (let ([new-board (play-move (world-m-obj w) (world-t w) (cons (cons bx by) (cons sx sy)) (world-gtb w))])
                                (make-world (car new-board)
                                            (cdr new-board)
                                            (if (not (winning-state (lposgen (cons bx by) (car new-board)))) (place-image (XorO (world-t w)) rx ry (world-db w))
                                                (let ([img (place-image (XorO (world-t w)) rx ry (world-db w))])
                                                  (if (world-t w) (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 0 255 120)) lbx lby img)
                                                      (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 255 0 120)) lbx lby img))))
                                            (not (world-t w))
                                            (bound-maker sx sy (cdr new-board))
                                            w
                                            ))]
                             [(undo-click x y (- (/ sidel 2) 60)) (if (null? (world-prev-world w))
                                                                      w (world-prev-world w))]
                             [(undo-click x y (+ (/ sidel 2) 60)) (begin (set! state 0) WORLDINIT1)]
                             [else w]
                             ))]
        [(= state 1)
         (let*([rx (round x)]
               [ry (round y)]
               [sx (rounds x)]
               [sy (rounds y)]
               [bx (roundb x)]
               [by (roundb y)]
               [lbx (- (+ es (* bigsqlength bx)) (/ bigsqlength 2))]
               [lby (- (+ es (* bigsqlength by)) (/ bigsqlength 2))])
    
           (begin (set! wcount (+ wcount 1))
                  (cond [(and (not (or (out-of-bound x) (out-of-bound y)))
                              (valid-move (car (world-bound w))
                                          (cdr (world-bound w)) bx by sx sy (world-gtb w) (world-m-obj w)))
                         (let ([new-board (play-move (world-m-obj w) (world-t w) (cons (cons bx by) (cons sx sy)) (world-gtb w))])
                                
                           (make-world (car new-board)
                                       (cdr new-board)
                                       (if (not (winning-state (lposgen (cons bx by) (car new-board)))) (place-image (XorO (world-t w)) rx ry (world-db w))
                                           (let ([img (place-image (XorO (world-t w)) rx ry (world-db w))])
                                             (if (world-t w) (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 0 255 120)) lbx lby img)
                                                 (place-image (rectangle bigsqlength bigsqlength 'solid (make-color 0 255 0 120)) lbx lby img))))
                                       (not (world-t w))
                                       (bound-maker sx sy (cdr new-board))
                                       w
                                       ))]
                        [(undo-click x y (- (/ sidel 2) 60)) (if (null? (world-prev-world w))
                                                                 w (world-prev-world (world-prev-world w)))]
                        [(undo-click x y (+ (/ sidel 2) 60)) (begin (set! state 0) WORLDINIT1)]
                        [else w]
                        ))
           ; (begin (board-maker wrld) (if (stopper wrld) (last-pic wrld) (void)) (make-decision wrld)) 
           )]
        [(= state 3)
         (cond [(undo-click x y (/ sidel 2)) (begin (set! state 0)
                                                    WORLDINIT1)]
               [else w])]
        [(= state 4)
         (cond [(undo-click x y (/ sidel 2)) (begin (set! state 0)
                                                    WORLDINIT1)]
               [else w])]
        ))         
;(define (win-gtb gtb)
;  (define (win-gtb-help gtb n list parity)
;    (if (= n 9) list
;        (if (= (list-ref gtb n) parity) (win-gtb-help gtb (+ n 1) (cons n list) parity)
;            (win-gtb-help (+ n 1) list parity))))
;  (cons (win-gtb-help gtb 0 '() 1) (win-gtb-help gtb 0 '() -1)))

(define (undo-click x y x-cord)
  (and (<= (abs (- x x-cord)) 40)
       (<= (abs (- y (- sidel (/ es 2)))) 20)))

(define (bound-maker sx sy gt) ;;output eg: (cons 1 3) or (cons #f #f)
  (if (lposgen (cons sx sy) gt) (cons #f #f) (cons sx sy)))

(define (valid-move boundx boundy currbx currby currsx currsy gt g)
  (cond [boundx (if (and (= boundx currbx) (= boundy currby)) (not (filled? currbx currby currsx currsy gt g)) #f)]
        [else (not (filled? currbx currby currsx currsy gt g))]))

(define (filled? bx by sx sy gt g)
  (if (lposgen (cons bx by) gt)
      #t
      (lposgen (cons sx sy) (lposgen (cons bx by) g))))

(define (out-of-bound s)
  (if (or (< s es) (> s (- sidel es)))
      #t
      #f))

(define (round s) (+ (/ sqlength 2) (* sqlength (quotient s sqlength))))
(define (XorO t) (if t X O))

(define (roundb s)
  (+ 1 (quotient (- s es) bigsqlength)))

(define (rounds s)
  (+ 1 (modulo (quotient (- s es) sqlength) 3)))

(define (stopper w)
  (number? (winning-state (world-gtb w))))


(define (last-pic w)
  (if (world-t w)
      (place-image winning-imageO (/ sidel 2) (/ es 2) (world-db w))
      (place-image winning-imageX (/ sidel 2) (/ es 2) (world-db w))))


(big-bang WORLDINIT1
  (to-draw board-maker sidel sidel)
  (stop-when stopper last-pic)
  (on-mouse click)
  )

