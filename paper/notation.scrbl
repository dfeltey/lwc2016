#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          scriblib/figure
          "mj-examples.rkt"
          (only-in scribble/manual racket racketblock code))
@(define (section title)
   (s:section #:tag (string-append "notation:" title) title))
@title[#:tag "notation"]{Notation: Tabular Notation}

In the "Notation" category, we have addressed the problem of "Tabular Notation" by extending the
syntax of MiniJava with a tabular syntax for specifying state machines. An example of one of these
tabular state machines is in @figure-ref["2d-state-machine"]. This state machine accepts transition
messages @code{zero} and one @code{one} and prints a number based on which transition is followed.
This example creates a class called @code{Receiver} with two methods, @code{zero} and
@code{one}. The initial state is @code{wait_1}.

@(figure*
  "2d-state-machine"
  "Tabular Notation for State Machines"
  @2d-state-machine)

@section{Assumptions}

The implementation uses Racket's exiting @racket[2d] parsing package, and relies on Racket's module
system to hygienically obtain the bindings used during expansion.

@section{Implementation}

@(figure*
  "2d-state-exped"
  "Partially Expanded State Machine"
  @2d-state-exped)

We extended the lexer with a new class of tokens for 2-dimensional tables. Those are recognized by
the @emph{#2dstate-machine} prefix, at which point the input stream is passed to an auxiliary
parser---Racket's existing 2d syntax---parser. After the 2d token is parsed, lexing resumes as
usual. This 2d parser will call back into the original parser to parse the contents of each cell.

The implementation extends the type checker with the new case to understand the new AST node for
state machines as a class. From here the macro for the state machine maps the different states to
integers and generates a class with a method for each transition function. And example of this for
the program in @figure-ref["2d-state-machine"] in is @figure-ref["2d-state-exped"].

Note that the bodies of the methods use the racket forms @racket[unless] and @racket[case]. Comping
into either MiniJava or directly to fully expanded Racket would make the macro that does this more
difficult to implement. However because Racket's syntax system allows macros to use any identifiers
that are bound in their environment, we can use these more convenient forms.

@section{Variants}

One possible variant of this problem would be a tabular @racket[if] or @code{switch} form, where
each cell is run if both of the tests in that row and column are true.

@section{Usability}

The tabular notation is difficult to use outside the DrRacket IDE. DrRacket contains key bindings
for extending cells within a table and centering the contents of cells. It also can convert boxes
drawn with ``-'', ``|'', and ``+'' into the Unicode notation.

@section{Impact}

Implementing this tabular notation required changes to each piece of the artifact. The lexer was
extended to recognized the new keyword and call into the existing 2d parser. This parser was a
private API, and so its implementation was copied into the solution. A new AST node was added to
represent the parsed form of the table. New type checking rules were added to type check this
@code{2dstate-machine} form. This new AST node and type checking rule were minor modifications, as
the new top-level form could be described in terms of existing types. A new macro was added to
compile the state machine into a combination of MiniJava and racket. This macro was able to share
many of the mechanisms used by type checking rule. Overall these changes added one clause the type
checker and one new macro to the MiniJava implementation, plus some shared code to handle extracting
information from the @code{2dstate-machine} AST node.

@section{Composability}

The addition of the @code{2dstate-machine} form coexists with the solutions to the other
benchmark problems. Our implementation of this tabular state machine notation should coexist with
other syntactic expansions to statements. The majority of the work for this extension is reusable by
other extensions using the same tabular notation.

@section{Limitations}

Convenient usage of the tabular notation is limited to the DrRacket IDE. As already mentioned, the
existing 2d parser did not publicly provide the API needed to extend the existing MiniJava parser
and had to be copied from the internals of the 2d package. The 2d package was designed to compose
with Racket readtable mechanism, which does not compose with generic parser automatically.

@section{Uses and Examples}

@;TODO citations/links?

This form of tabular notation is used by Racket's @code{2dcond} and @code{2dmatch} forms. These
forms are used in the implementation of Redex.

@section{Effort}

TODO:parsing. Once parsing was completed the remainder of the work required about 140 lines of code
and took 2-3 hours.

@;; Maybe leave this out
@;; @section{Other Comments}
