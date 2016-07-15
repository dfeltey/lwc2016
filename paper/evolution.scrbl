#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          (only-in scribble/manual racket racketblock))
@(define (section title)
   (s:section #:tag (string-append "evolution:" title) title))

Our solution to the benchmark problem "Beyond-Grammar Restrictions" extends MiniJava with a @racket[break] keyword,
that is only valid within @racket[while] loops.


@section{Assumptions}
@section{Implementation}
@section{Variants}
@section{Usability}
@section{Impact}
@section{Composability}
@section{Limitations}
@section{Uses and Examples}
@section{Effort}
@;; Maybe leave this out
@section{Other Comments}

