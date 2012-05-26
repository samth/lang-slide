#lang scheme
(provide langs-pict
         langs-in-tree
         langs-with-colors)
(require "draw-plain.ss"
         slideshow slideshow/code
         scheme/runtime-path)
(define-runtime-path lang-colors.rkt "lang-colors.rkt")

(define (color->name c)
  (let-values ([(r g b) (split-out-color c)])
    (cond
      [(and (= r 0) (= g 0) (= b 0)) 'black]
      [(and (= r g) (= r b)) 'gray]
      [(and (= 255 b) (= r g)) 'blue]
      [(and (= r 0) (= g 0)) 'blue]
      [(and (= r 0) (= b 0)) 'green]
      [(and (= g 0) (= b 0)) 'red]
      [else 'other])))

(define (color-name->index c)
  (case c
    [(blue) 0]
    [(red) 1]
    [(green) 2]
    [(gray) 3]
    [(other) 4]
    [(black) 5]
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

(define lang-colors 
  (sort (call-with-input-file lang-colors.rkt read)
        color<=?
        #:key cadr))

(define-values (black-langs colored-langs)
  (partition (λ (x) (equal? (cadr x) "#000000")) lang-colors))

(define (line->color cl)
  (parameterize ([current-font-size 16])
    (hc-append 6
               (colorize (filled-ellipse 14 14)
                         (string->color (cadr cl)))
               (text (car cl) (current-code-font) (current-font-size)))))

(define (langs-pict color? #:picts [p (if (pict? color?) (list color?) (list))])
  (define colors (langs-with-colors))
  (define len (length colors))
  (define start (ceiling (/ len 2)))
  (define-values (one two) (split-at colors start))
  (ht-append
   0
   (langs-in-tree color?)
   (apply vc-append 40 
          (ht-append 20
                     ((if color? values ghost)
                      (apply vl-append 2 one))
                     ((if color? values ghost)
                      (apply vl-append 2 two)))
          p)))

(define (langs-with-colors)
  (map line->color
       (append colored-langs (list (list "everything else" "#000000")))))

(define (langs-in-tree color?)
  (inset (lang-pict 550 color?) 14 -30 10 10))
  
;(slide (langs-pict #f)) (slide (langs-pict #t))
