#lang scribble/manual
@require[@for-label[lang-slide/pictures
                    racket/base]
         lang-slide/pictures
         ;slideshow/pict
         ]

@title{A picture showing all the languages used to implement Racket.}
@author{Sam Tobin-Hochstadt (Originally by Robby Findler)}

Source code: @url{https://github.com/samth/lang-slide}

Here is a bird's eye view of the modules implementing racket:

@(langs-pict #f)

And here is the languages they use:

@(langs-pict #t)

@section{Pictures}

@racketmodname{lang-slide/pictures} provides several 
@racketmodname[pict]s, with some options.

@defproc[(langs-pict [color? any]
          [#:fit fit (-> pict? pict?)]
          [#:picts p (listof pict-convertible?)])
         pict?]{
}
@defproc[(langs-in-tree [color? boolean?])
         pict?]{
}
@defproc[(langs-with-colors)
         (listof pict?)]{
}

@section{Slides}

To show a slide that demos the pict, run
@tt{main.rkt} on the command-lide, or use

@racketblock[
 @#,hash-lang[] @#,racketmodname[racket]
 (require (submod lang-slide/main main))]

@tt{hudak-quote.rkt} has a slideshow slide that uses
the picture along with a quote from Paul Hudak.

@section{PNG}

@tt{mk-img.rkt} generates a PNG of the image.

@section{Regenerating the data}

@tt{find.rkt} regenerates @tt{lang.plain} and
@tt{lang-colors.rktd} automatically when run.
