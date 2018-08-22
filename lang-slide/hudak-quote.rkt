#lang racket/base
(require "main.rkt"
         slideshow
         slideshow/code
         unstable/gui/slideshow)

(provide langs hudak-quote perlis-quote)


(define hudak-quote
  (vr-append 10 (vl-append (t "“A domain specific language is the ultimate abstraction.” "))
             (t "    — Paul Hudak")))

(define perlis-quote 
  (vr-append 10 (vr-append (t "“There will always be things we wish to say in our programs") 
                           (t "that in all known languages can only be said poorly.”"))
             (t "   — Alan Perlis")))

(define p2 (vl-append (t "Racket ships more than") (t "40 documented languages")))
(define p1 (lt-superimpose 
            (ghost p2)
            (vl-append (t "In 6000+ files of") (t "Racket source code ..."))))

(define (langs)
  (define p1+p2 (vl-append 10 p1 p2))
  (parameterize ([current-code-font 'default])
    (slide/staged [hudak one two] 
                  ;#:title "Files in Racket" 
                  ;#:layout 'tall
                  (cond 
                    [(eq? stage-name 'hudak)
                     (mini-slide (vr-append 60 hudak-quote perlis-quote))]
                    [(eq? stage-name 'one)
                     (values (langs-pict #f #:picts (list (lt-superimpose p1 (ghost p1+p2)))))]
                    [else
                     (langs-pict p1+p2)]))))

(module+ main 
  (langs))

