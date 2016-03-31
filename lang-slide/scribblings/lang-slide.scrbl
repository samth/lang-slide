#lang scribble/manual
@require[@for-label[lang-slide
                    racket/base]
         lang-slide/pictures
         ;slideshow/pict
         ]

@title{A picture showing all the languages used to implement Racket.}

Source code: @url{https://github.com/samth/lang-slide}

Here is a bird's eye view of the modules implementing racket:

@(langs-pict #f)

And here is the languages they use:

@(langs-pict #t)
