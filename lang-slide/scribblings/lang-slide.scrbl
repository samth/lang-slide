#lang scribble/manual
@require[@for-label[lang-slide/pictures
                    racket/base]
         lang-slide/pictures
         ;slideshow/pict
         ]

@title{A picture showing all the languages used to implement Racket.}
@author{Sam Tobin-Hochstadt (Originally by Robby Findler)}

Source code: @url{https://github.com/samth/lang-slide}

Here is a bird's eye view of the modules implementing the Racket
distribution, as organized by the filesystem structure (so the root in
the middle corresponds to a directory containing the Racket
distribution, and subtrees are nested directories):

@(langs-pict #f #:layout 'center)

And here are the languages they use:

@(langs-pict #t #:layout 'center)

@section{Pictures}

@defmodule[lang-slide/pictures]

@racketmodname{lang-slide/pictures} provides several 
@racketmodname[pict]s, with some options.

@defproc[(langs-pict [color? any]
          [#:fit? fit? any/c #f]
          [#:picts picts (listof pict-convertible?) (if (pict? color?) (list color?) null)]
          [#:more? more? any/c #f]
          [#:layout layout (or/c '20% center) '20%])
         pict?]{

If @racket[color?] is @racket[#f], graph nodes are all gray, and the
table of colors is not shown (although space for the table is included
in the result pict).

Extra picts in @racket[picts] are added to the bottom of the table of
colors. For historical reasons, if @racket[color?] is a pict, it is
put into a list for the default value of @racket[picts].

If @racket[more?] is true, then the picture includes additional
languages provided by packages that have been installed on an example
Racketeer's machine.

The @racket[layout] argument determines how the filesystem graph is
aligned with respect to the table of language colors. The
@racket['20%] mode top-aligns the graph and table, but puts a margin
above the graph that is 20% of its size. The @racket['center] mode
center-aligns the plot and table.

@history[#:changed "1.1" @elem{Added the @racket[#:more?] and @racket[#:layout] arguments.}]}

@defproc[(langs-in-tree [color? boolean?]
                        [#:more? more? any/c #f])
         pict?]{

Just the graph of modules.

@history[#:changed "1.1" @elem{Added the @racket[#:more?] argument.}]}


@defproc[(langs-with-colors [#:more? more? any/c #f])
         (listof pict?)]{

Just the table of language colors.

@history[#:changed "1.1" @elem{Added the @racket[#:more?] argument.}]}


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
