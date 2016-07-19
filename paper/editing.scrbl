#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          (only-in scribble/manual racket racketblock))
@(define (section title)
   (s:section #:tag (string-append "editing:" title) title))
@title[#:tag "editing"]{Editing: Restructuring}
In this section we present our solution to the "Restructuring" benchmark problem
from the "Editing" category. We address the problem of restructuring through the
implementation of a refactoring tool for MiniJava. Our refactoring tool restructures
MiniJava @racket[if] statements by swapping the @emph{then} and @emph{else} branches
and negating the condition. Because this refactoring modifies the syntax of the original
program, particularly adding the negation, the tool must take care to produce a syntactically
valid program when the refactoring applies.

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
@;; @section{Other Comments}

