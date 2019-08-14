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
                     #:picts [p (if (pict? color?) (list color?) (list))]
                     #:more? [more? #f]
                     #:layout [layout '20%]) ; '20% or 'center
  (langs-pict color?
              #:fit (λ (all)
                      (if fit?
                          (scale all (min 1 
                                          (/ client-w (pict-width all))
                                          (/ client-h (pict-height all))))
                          all))
              #:picts p
              #:more? more?
              #:layout layout))

(module+ main
  (for* ([more? '(#f #t)]
         [color? '(#f #t)])
    (slide (langs-pict1 color? #:fit? #t #:more? more? #:layout 'center))))
