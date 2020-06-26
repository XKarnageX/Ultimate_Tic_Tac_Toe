#lang racket
(require 2htdp/image
         2htdp/universe)
(provide (all-defined-out))

(define background "background.png")
(define factor 3/4)
(define backg (scale factor (bitmap/file background)))

(define sidel 600)
(define gap (/ sidel 5))
(define font 40)
(define colorr 'white)
(define back (place-image (text "Back" 30 colorr)  40 20
                          (rectangle 80 40 'outline colorr)))

(define 1p (text "Single Player" font colorr))
(define 2p (text "Two Player" font colorr))
(define howto (text "How To Play" font colorr))
(define credits (text "Credits" font colorr))

(define mainbackg (place-image credits (/ sidel 2) (* 4 gap)
                               (place-image howto (/ sidel 2) (* 3 gap)
                                            (place-image 2p (/ sidel 2) (* 2 gap)
                                                         (place-image 1p (/ sidel 2) gap backg)))))


(define howto-text (text "Win three games of Tic Tac Toe in a row.
You may only play in the big field that
corresponds to the last small field your
opponent played. When your are sent to a
field that is already decided, you can
choose freely." 30 colorr))

;;(define howto-gif (bitmap/file "uttt.gif"))

(define howto-backg
  (place-image back (/ sidel 2) (- sidel (/ gap 2)) 
               (place-image (text "How To Play" 40 colorr) (/ sidel 2) gap 
                            (place-image howto-text (/ sidel 2) (/ sidel 2) backg))))

(define createdby
  (text "Mohammad Taufeeque
180050062
Arjun Kashettiwar
180050012
Utkarsh Agarwal
180050115" 25 colorr))

(define acknowledgement (text "Background image source:
https://www.brushlovers.com/vector/high-tech-abstract-green-background.html
" 17 colorr))



(define credits-backg
  (place-image back (/ sidel 2) (- sidel (/ gap 2)) 
               (place-image (text "Created By: " 40 colorr) (/ sidel 2) (/ gap 2)
                            (place-image createdby (/ sidel 2) (* 1.6 gap)  
                                         (place-image acknowledgement (/ sidel 2) (* 3.2 gap)
                                                      (place-image (text "Acknowledgement" 40 colorr) (/ sidel 2) (* 2.7 gap) backg))))))
