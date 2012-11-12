#lang scheme/gui
(require "main.rkt" slideshow)

(define the-margin 32)

(define the-pict (let ([p (langs-pict #t)])
                   (scale p (/ (- 1024 the-margin) (pict-width p)))))
(pict-width the-pict)
(define bm (make-object bitmap% 
             (+ the-margin (ceiling (inexact->exact (pict-width the-pict))))
             (+ the-margin (ceiling (inexact->exact (pict-height the-pict))))))
(define bdc (make-object bitmap-dc% bm))
(send bdc set-smoothing 'aligned)
(send bdc clear)
(draw-pict the-pict bdc (/ the-margin 2) (/ the-margin 2))
(send bdc set-bitmap #f)
(send bm save-file "langs.png" 'png)
