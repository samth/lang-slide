#lang scheme

(provide (rename-out [langs-pict1 langs-pict])
         langs-in-tree
         langs-with-colors)
(require "pictures.rkt")
(require "draw-plain.ss"
         "orig-colors.rkt"
         racket/draw
         slideshow/code
         scheme/runtime-path
         slideshow/pict
         slideshow/base
         "pictures.rkt")

(define (langs-pict1 color? 
                     #:fit? [fit? #f]
                     #:picts [p (if (pict? color?) (list color?) (list))])
  (langs-pict color?
              #:fit (λ (all)
                      (if fit?
                          (scale all (min 1 
                                          (/ client-w (pict-width all))
                                          (/ client-h (pict-height all))))
                          all))
              #:picts p))

(module+ main
  (slide (langs-pict1 #f #:fit? #t))
  (slide (langs-pict1 #t #:fit? #t)))
