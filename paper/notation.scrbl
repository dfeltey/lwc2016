#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          scriblib/figure
          "mj-examples.rkt"
          (only-in scribble/manual racket racketblock code))
@(define (section title)
   (s:section #:tag (string-append "notation:" title) title))
@title[#:tag "notation"]{Notation: Tabular Notation}

In the "Notation" category, we have solved the notion of "Tabular Notation" by extending the
syntax of MiniJava with a tabular syntax for specifying state machines. An example of one of these
tabular state machines is in @figure-ref["2d-state-machine"]. This state machine accepts transition
messages @code{zero} and @code{one} and prints a number based on which transition is followed.
This example creates a class called @code{Receiver} with two methods, @code{zero} and
@code{one}. The initial state is @code{wait_1}.

@(figure*
  "2d-state-machine"
  "Tabular Notation for State Machines"
  @2d-state-machine)

@section{Assumptions}

The implementation uses Racket's existing @racket[2d] parsing package to parse the tabular
notation. In addition the implementation requires that the state machine be fully connected.


@section{Implementation}

@(figure
  "2d-state-exped"
  "Partially Expanded State Machine"
  @2d-state-exped)

Implementing this extension required changes to each phase of the MiniJava implementation. The
parser and lexer were extended with the 2d syntax. The parsed table is then passed through each
layer until it reaches the Macro Expansion phase. There a new @racket[2dstate-machine] macro
transforms it into a MiniJava class.

The lexer was extended with a new class of tokens for 2-dimensional tables. Those are recognized by
the @emph{#2dstate-machine} prefix, at which point the input stream is passed to an auxiliary
parser---Racket's existing 2d syntax parser, which handles parsing the 2d table. After the 2d table
is parsed, lexing resumes as usual. This 2d parser will call back into the original parser to parse
the contents of each cell.

The type checker with the new case to understand the new AST node for
state machines as a class. From here the @racket[2dstate-machine] macro maps the different states to
integers and generates a class with a method for each transition function. And example of this for
the program in @figure-ref["2d-state-machine"] in is @figure-ref["2d-state-exped"]. This expanded
code had the @racket['disappeared-binding] property placed on it to enable renaming of the states.

Note that the bodies of the methods use the Racket forms @racket[unless] and @racket[case]. This
expansion give us reuse of both racket and MiniJava forms. Racket's expansion model allows for this
mixing of languages during expansion.

Compiling
into either MiniJava or directly to fully expanded Racket would make the macro that does this more
difficult to implement. However because Racket's syntax system allows macros to use any identifiers
that are bound in their environment, we can use these more convenient forms.

@section{Variants}

One possible variant of this problem would be a tabular @racket[if] or @code{switch} form, which
would dispatch on the tests on the boarders of its table. A cell would run when the two tests on in
that row and column are true.

@section{Usability}

The tabular notation is difficult to use outside the DrRacket IDE. DrRacket contains key bindings
for extending cells within a table and centering the contents of cells. It also can convert boxes
drawn with ``-'', ``|'', and ``+'', which are more convenient to type, into the Unicode
body-draw-character notation.

@section{Impact}

Implementing this tabular notation required changes to each piece state of the MiniJava
implementation. The lexer was extended to recognize the new keyword and call into the existing 2d
parser. Using this parser required one of its private API's, so its implementation was copied into
the solution.

From here each phase was extended to recognize and pass along the state machine for to the macro
expansion layer. A new macro was added to compile the state machine into a combination of MiniJava
and racket.

@section{Composability}

The @code{2dstate-machine} form coexists with the solutions to the other benchmarks. The state
machine notation should also coexist with other syntactic extensions to statements. Future
extensions using the same tabular notation can reuse the majority of the implementation.

@section{Limitations}

Convenient usage of the tabular notation is limited to the DrRacket IDE.

The 2d parser package provided by Racket is designed to extend a language's syntax via a mechanism
called read tables. MiniJava's parser does not support read tables, so interfacing with the 2d
parser required using one of its internal API's and modifying the MiniJava lexer to call into this
API.

@section{Uses and Examples}

@;TODO citations/links?

This form of tabular notation is used by Racket's @code{2dcond} and @code{2dmatch} forms. These
forms are used in the implementation of Redex.

@section{Effort}

Adding the 2d capabilities to the existing parser took 1-2 hours. Once parsing was completed, the
remainder of the work required about 140 lines of code and took 2-3 hours.

@;; Maybe leave this out
@;; @section{Other Comments}
