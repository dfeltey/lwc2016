#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          scriblib/figure scriblib/footnote
          "mj-examples.rkt"
          (only-in scribble/manual racket racketblock code))
@(define (section title)
   (s:section #:tag (string-append "notation:" title) title))
@title[#:tag "notation"]{Notation: Tabular Notation}

In the @emph{Notation} category, we tackle the problem of @emph{Tabular Notation} by extending MiniJava with a
syntax for specifying state machines, as seen in @figure-ref["2d-state-machine"].  A state machine
compiles into a class, whose name is in the top left cell.  The top row lists the state names.  The
left column lists the transition methods, whose bodies are in the inner cells, followed by the next
state.

@(figure*
  "2d-state-machine"
  "Tabular Notation for State Machines"
  @2d-state-machine)

@section{Assumptions}

The implementation uses Racket's existing @racket[2d] parsing package to parse the tabular
notation. In addition the implementation requires that every state allow every transition.

@section{Implementation}

Implementing this extension required changes to each phase of the MiniJava implementation.  First,
we extend the lexer with a new class of tokens for 2-dimensional tables, which are recognized by the
@emph{#2dstate-machine} prefix. For this token the lexer uses Racket's existing 2d syntax parser, to
find the bounds of the table and break it into separate cells.  The original parser then handles the
contents of each cell. After a 2d table is parsed, lexing resumes as usual.

After parsing, each table is a syntax object that corresponds to an invocation of the
@racket[2dstate-machine] macro. We then extend the following phases to recognize these objects and
pass them along.  The @racket[2dstate-machine] macro compiles tables to classes.  Each transition
becomes a method which dispatches on the state, represented by an integer field.

@section{Variants}

One possible variant of this problem would be a tabular @racket[if] or @code{switch} form, which
would dispatch on two scrutinees, one selecting a row, the other a column.

@section{Usability}

DrRacket provides special support for inputting and editing this tabular syntax. It is
difficult to use outside DrRacket.

@section{Impact}

In addition to the aforementioned changes to the lexer, our use of the 2d parser depends on one of
its private API's, therefore we copied its implementation.  From there each phase was extended to
recognize and pass along the state machine to Racket's macro expander and the macro
@racket[2dstate-machine] was added to compile the state machines.

@section{Composability}

The @code{2dstate-machine} form coexists with the solutions to the other benchmarks, and would also
compose with syntactic extensions to statements. Future extensions using the same tabular notation
can reuse the majority of the implementation.

To compose with the @emph{check syntax} tool and enable reasoning about state name as bindings, the
@racket[2dstate-machine] macro uses the @racket['disappeared-binding] syntax property.

@section{Limitations}

Convenient usage of the tabular notation is limited to DrRacket.

@;TODO citation

Racket's 2d parser relies on @emph{read tables}
@note{Racket's read tables descend from Common Lisp and MacLISP, a modern Racket-specific treatment of read tables is found in} @;; CITE
to extend the reader. MiniJava's reader does not support
read tables, which led to using one of the 2d parser's internal APIs.


@section{Uses and Examples}

@;TODO citations/links?

This form of tabular notation is used by Racket's @code{2dcond} and @code{2dmatch} forms. These
forms are used in the implementation of Redex.

@section{Effort}

Adding the 2d capabilities to the existing parser took 1-2 hours. The
remaining work required about 140 lines of code and took 2-3 hours.

@;; Maybe leave this out
@;; @section{Other Comments}
