#lang racket

(require "draw-plain.ss"
         "orig-colors.rkt"
         racket/draw
         racket/promise
         slideshow/code-pict
         racket/runtime-path
         slideshow/pict)

(provide langs-pict
         langs-in-tree
         langs-with-colors)

(define-runtime-path lang-colors.rktd "lang-colors.rktd")
(define-runtime-path more-lang-colors.rktd "more-lang-colors.rktd")

(define (color->name c)
  (define-values (r g b) (split-out-color c))
  (cond
    [(and (equal? r 0) (equal? g 0) (equal? b 0))
     'black]
    [else
     (define res 
       (for/or ([(k v) (in-hash orig-colors)])
         (for/or ([c (in-list v)])
           (define rgb (cond
                         [(string? c)
                          (define clr (send the-color-database find-color c))
                          (list (send clr red) (send clr green) (send clr blue))]
                         [else 
                          c]))
           (and (equal? rgb (list r g b))
                k))))
     (unless res (error 'color->name "unable to find color name for ~s" c))
     res]))

(define (color-name->index c)
  (case c
    [(blue) 0]
    [(red) 1]
    [(orange) 1.5]
    [(green) 2]
    [(gray) 3]
    [(pink) 4]
    [(cyan) 5]
    [(purple) 5.5]
    [(yellow) 7]
    [(brown) 8]
    [(black) 100]
    [else (error 'color-name->index "unk ~s" c)]))

(define (split-out-color c)
  (values
   (string->number (substring c 1 3) 16)
   (string->number (substring c 3 5) 16)
   (string->number (substring c 5 7) 16)))

(define (color<=? c1 c2)
  (let ([n1 (color->name c1)]
        [n2 (color->name c2)])
    (cond
      [(equal? n1 n2)
       (string<=? c1 c2)]
      [else
       (<= (color-name->index n1)
           (color-name->index n2))])))

(define (get-colors lang-colors.rktd)
  (define lang-colors 
    (sort (call-with-input-file lang-colors.rktd read)
          color<=?
          #:key cadr))

  (define-values (black-langs colored-langs)
    (partition (λ (x) (equal? (cadr x) "#000000")) lang-colors))

  colored-langs)

(define (line->color cl)
  (let ([font-size 14])
    (hc-append 6
               (colorize (filled-ellipse 14 14)
                         (string->color (cadr cl)))
               (text (car cl) (current-code-font) font-size))))

(define (langs-pict color?
                    #:more? [more? #f]
                    #:layout [layout '20%] ; '20% or 'center
                    #:fit [fit (λ (x) x)]
                    #:picts [p (if (pict? color?) (list color?) (list))])
  (define colors (langs-with-colors #:more? more?))
  (define len (length colors))
  (define start (ceiling (/ len 2)))
  (define-values (one two) (split-at colors start))
  (define the-graph (langs-in-tree color? #:more? more?))
  (define color-table
    (apply vc-append 40 
           (ht-append 30
                      ((if color? values ghost)
                       (apply vl-append 2 one))
                      ((if color? values ghost)
                       (apply vl-append 2 two)))
           p))
  (define sep 20)
  (define all
    (case layout
      [(20%)
       (ht-append sep
                  (vc-append (blank (round (* 1/5 (pict-height the-graph)))) the-graph)
                  color-table)]
      [(center)
       (hc-append sep
                  the-graph
                  color-table)]))
  (fit all))

(define colored-langs (delay (get-colors lang-colors.rktd)))
(define more-colored-langs (delay (get-colors more-lang-colors.rktd)))

(define (langs-with-colors #:more? [more? #f])
  (map line->color
       (append (force (if more? more-colored-langs colored-langs))
               (list (list "everything else" "#000000")))))

(define (langs-in-tree color? #:more? [more? #f])
  (inset (lang-pict 550 color? #:more? more?) 14 10 -10 10))
