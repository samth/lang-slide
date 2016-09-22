#lang racket ;scheme/gui
(provide lang-pict string->color)

(require scheme/runtime-path
         slideshow/pict
         racket/draw)

(define-runtime-path lang.plain "lang.plain")

(define (parse-file)
  (call-with-input-file lang.plain
    (λ (port)
      (for ([l (in-lines port)]) 
        (parse-line l)))))

;; nodes : hash[string -o> node]
(define nodes (make-hash))
(define-struct node (x y w h type color) #:transparent)

;; parents : hash[string -o> string]
(define parents (make-hash))

(define graph-width 0)
(define graph-height 0)

(define (parse-line line)
  (cond
    [(regexp-match #rx"^node \"([^\"]*)\" +([0-9.]*) +([0-9.]*) +([0-9.]*) +([0-9.]*) +\"([^\"]*)\" +([^ ]*) +([^ ]*) +([^ ]*) +([^ ]*)"
                   line)
     =>
     (λ (m)
       (let-values ([(id x y w h label type1 type2 color1 color2)
                     (apply values (cdr m))])
         (hash-set! nodes id (make-node (string->number y)
                                        (string->number x)
                                        (string->number w)
                                        (string->number h)
                                        (string->symbol type2)
                                        (string->color color1)))))]
    [(regexp-match #rx"^edge \"([^\"]*)\" +\"([^\"]*)\""
                   line)
     =>
     (λ (m) 
       (let-values ([(src dest) (apply values (cdr m))])
         (hash-set! parents dest src)))]
    [(regexp-match #rx"^graph ([0-9.]*) ([0-9.]*) ([0-9.]*)" line) 
     =>
     (λ (m) 
       (let-values ([(scale w h) (apply values (cdr m))])
         (set! graph-width (string->number w))
         (set! graph-height (string->number h))))]
    [(regexp-match #rx"^stop" line) (void)]
    [else 
     (error 'parse-line "unknown line ~s\n" line)]))

(define (string->color str)
  (cond
    [(regexp-match
      #rx"#([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])([0-9a-f][0-9a-f])"
      str)
     =>
     (λ (m)
       (let-values ([(r g b) (apply values (cdr m))])
         (make-object color% 
           (string->number r 16)
           (string->number g 16)
           (string->number b 16))))]
    [else
     (let ([c (send the-color-database find-color str)])
       (unless c
         (error 'string->color "unknown color ~s" str))
       c)]))

(define (draw-graph dc dx dy w h color?)
  (let ([scale (min (/ w graph-width)
                    (/ h graph-height))])
    (define (draw-node name node)
      (case (node-type node)
        [(circle)
         (let-values ([(nx ny) (node->xy node)]
                      [(px py) (node->xy (hash-ref nodes (hash-ref parents name)))])
           (let ([nw (* 1.8 (node-w node))]
                 [nh (* 1.8 (node-h node))])
             (cond
               [color?
                (send dc set-pen "black" 1 'transparent)
                (send dc set-brush (node-color node) 'solid)]
               [else
                (send dc set-pen "SlateGray" 1 'solid)
                (send dc set-brush "LightSlateGray" 'solid)])
             (send dc draw-ellipse 
                   (+ dx (- nx (* scale (/ nw 2))))
                   (+ dy (- ny (* scale (/ nh 2))))
                   (* scale nw)
                   (* scale nh))))]
        [else (void)]))
    (define (draw-edge src dest)
      (send dc set-pen "gray" 1 'solid)
      (send dc set-brush "black" 'transparent) 
      (let-values ([(sx sy) (node->xy (hash-ref nodes src))]
                   [(tx ty) (node->xy (hash-ref nodes dest))])
        (send dc draw-line
              (+ dx sx)
              (+ dy sy)
              (+ dx tx)
              (+ dy ty))))
    
    (define (node->xy node)
      (values (* scale (node-x node))
              (- h (* scale (node-y node)))))
    (let ([smoothing (send dc get-smoothing)]
          [pen (send dc get-pen)]
          [brush (send dc get-brush)])
      (send dc set-smoothing 'aligned)
      (hash-for-each
       parents
       (λ (dest src)
         (draw-edge src dest)))
      (for-each
       (λ (name-node)
         (draw-node (car name-node)
                    (cadr name-node)))
       (sort (hash-map nodes list)
             (compare-name-node-list w h)))
      (send dc set-smoothing smoothing)
      (send dc set-pen pen)
      (send dc set-brush brush))))

(define ((compare-name-node-list w h) name-node1 name-node2)
  (let* ([c (make-rectangular (/ w 2) (/ h 2))]
         [x (make-rectangular (node-x (cadr name-node1))
                              (node-x (cadr name-node2)))]
         [y (make-rectangular (node-y (cadr name-node1)) 
                              (node-y (cadr name-node2)))]
         [ax (angle (- x c))]
         [ay (angle (- y c))])
    (cond
      [(= ax ay)
       (< (magnitude x) (magnitude y))]
      [else
       (< ax ay)])))

(parse-file)

#;
(begin
  (define f (new frame% [label ""]))
  (define c (new canvas% 
                 [parent f] 
                 [paint-callback
                  (λ (c dc) 
                    (let-values ([(w h) (send c get-client-size)])
                      (draw-graph dc 0 0 w h)))]))
  (send f show #t))

(define (lang-pict size color?)
  (dc (λ (dc dx dy) (draw-graph dc dx dy size size color?))
      size size))
