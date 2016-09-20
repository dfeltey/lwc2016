#lang scribble/sigplan @10pt

@(require (prefix-in s: scribble/base)
          scriblib/figure scriblib/footnote
          "mj-examples.rkt"
          "bib.rkt"
          (only-in scribble/manual racket racketblock code))

@(define (section title) (s:section #:tag (string-append "notation:" title) title))

@; -----------------------------------------------------------------------------
@title[#:tag "notation"]{Notation: Tabular Notation}

Adding notation to our implementation of MiniJava is quite
straightforward. To illustrate this idea with a rather extreme example, we
present here the result of tackling the problem of @emph{Tabular Notation}
from the @emph{Notation} category. Specifically, we explain how to add
tabular notation to MiniJava for specifying state machines via tables.

@(figure*
  "2d-state-machine"
  "Tabular notation for state machines"
  @2d-state-machine)

@Figure-ref["2d-state-machine"] presents an example state machine. The syntax is purely
textual, relying on Racket's Unicode integration. A programmer produces the
table outline with Unicode characters.
In the context of MiniJava, the table represents a two-dimensional grid of
transitions. Our implementation understands it as an alternative notation
for a class. This synthesized class implements the corresponding state
machine.

In @figure-ref{2d-state-machine}, the first row in the table specifies the
names of the states: @racket[wait_0] and @racket[wait_1]. The first column
specifies the names of the input symbols: @racket[zero] and
@racket[one]. The cells in the middle portion of the diagram specify what
happens in the given state when the given symbol is received; each cell
contains some arbitrary MiniJava code that runs for its effect, followed by
the name of a new state to transition to. For example, when in the
@racket[wait_0] state, if the @racket[zero] input comes, then the state
machine will print out @tt{0} and transition to the @racket[wait_1] state.
The state machine is reified as a MiniJava class, named by the single name
in the upper-left cell: @racket[Receiver]. The inputs to the state machine
are reified as nullary methods on the class.

Finally, the second class in @figure-ref["2d-state-machine"] shows a client
of the state machine class. The @racket[StateMachineRunner] class is a
textual class definition. It refers to the state machine by name and
creates an instance. Following that, it sends this state machine four
inputs via method calls. 

@section{Assumptions}

The implementation uses Racket's existing @racket[2d] parsing package to parse the tabular
notation. In addition the implementation requires that every state allow every transition.

@section{Implementation}

Implementing this extension requires changes to each phase of our MiniJava implementation.  First,
we extend the lexer with a new class of tokens for 2-dimensional tables, which begin with the token
@tt{#2dstate-machine}. For this token, the lexer uses Racket's existing 2d syntax parser to
find the bounds of the table and break it into separate cells.  The original parser then handles the
contents of each cell. After parsing a  2d table, lexing resumes as usual.

After parsing, each table is a syntax object that corresponds to an invocation of the
@racket[2dstate-machine] macro. We then extend the following phases to recognize these objects and
pass them along.  The @racket[2dstate-machine] macro compiles tables to classes.  Each transition
becomes a method which dispatches on the state, represented by an integer field.

@section{Variants}

One possible variant of this problem would be a tabular @racket[if] or @code{switch} form, which
would dispatch on two scrutinees, one selecting a row, the other a column.

@section{Usability}

DrRacket provides special support for inputting and editing this tabular syntax.@note{See DrRacket's manual for more:
@url{http://docs.racket-lang.org/drracket/Keyboard_Shortcuts.html#(idx._(gentag._219._(lib._scribblings/drracket/drracket..scrbl)))}}
It is difficult to use outside of DrRacket.

@section{Impact}

In addition to the aforementioned changes to the lexer, each phase is extended to
recognize and pass along the state machine and the macro
@racket[2dstate-machine] is added to compile the state machines.

@section{Composability}

The @code{2dstate-machine} form coexists with the solutions to the other benchmarks, and would also
compose with syntactic extensions to statements. Future extensions using the same tabular notation
can reuse the majority of the implementation.

The @racket[2dstate-machine] macro cooperates with DrRacket's @emph{check syntax} tool to
recognize state names as bindings.

@section{Limitations}
Convenient usage of the 2d syntax is limited to DrRacket.

Racket's 2d parser relies on
@emph{read tables}@note{Racket's read tables descend from Common
 Lisp@~cite[commonlisp] and MacLISP@~cite[readtables], a modern Racket-specific treatment is found at
 @url{http://docs.racket-lang.org/reference/readtables.html}}
to extend the reader. MiniJava's reader does not support
read tables, which led to using one of the 2d parser's internal APIs.


@section{Uses and Examples}

@;TODO citations/links?

This form of tabular notation is used by Racket's @code{2dcond} and @code{2dmatch} forms. These
forms are used in the implementation of Redex@~cite[redex-book].

@section{Effort}

Adding the 2d capabilities to the existing parser took 1-2 hours. The
remaining work required about 140 lines of code and took 2-3 hours. Work on
editing support for the tabular notation is ongoing.

@;; Maybe leave this out
@;; @section{Other Comments}
