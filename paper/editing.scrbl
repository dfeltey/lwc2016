#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          (only-in scribble/manual racket racketblock)
          scriblib/figure
          "mj-examples.rkt")
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
Our refactoring tool assumes that the object language, MiniJaa, provides all the necessary information
needed to perform the refactoring. For the @racket[if] restructuring this means that the MiniJava implementation
must retain the source locations corresponding to each piece of a MiniJava @racket[if] statement. More specifically,
we require that this information be stored in syntax property with the key @racket['refactor] in order for our tool
to find possible opportunities for refactoring.

@section{Implementation}
@(figure*
  "refactor-prop"
  "The syntax property used to implement the refactoring tool"
  refactor-impl)
@;; TODO: need example use of the refactoring, probably DrRacket screenshots would be best
Syntax objects and the ability to attach arbitrary information to them using Racket's syntax properties
were introduced in @secref{racket-overview} and further explored in @secref{drracket} as a way for
languages written in Racket to communicate with external tools. To implement the @racket[if] restructuring
tool we have used syntax properties to attach information about the source location of a MiniJava @racket[if]
statement that will persist in a program's fully expanded syntax. @Figure-ref{refactor-prop} shows the implementation
of MiniJava's @racket[if] statement. In order to enable our refactoring tool to operate on both regular MiniJava and
parenthesized MiniJava programs we require that the MiniJava parser also attach a syntax property to indicate that the
program was written in MiniJava's concrete syntax. The provenance of the @racket[if] statement is necessary because the
syntax of conditions in @racket[if] statements in MiniJava and parenthesized MiniJava differ, and the same program
transformation would not be valid in both languages.

Our tool is implemented as a plugin for DrRacket, similar to the check-syntax tool discussed briefly in @secref{drracket}.
The tool processes a program's fully-expanded syntax and performs a depth-first traversal of the syntax tree. The source locations
of syntax objects that have the @racket['refactor] property attached are stored, along with the value of the syntax property,
and later used to determine where the @racket[if] refactoring applies within a MiniJava program. As @figure-ref{refactor-prop} shows,
the @racket['refactor] syntax property contains the source locations of each part of an @racket[if] statement, this is all the
information necessary to swap the @emph{then} and @emph{else} branches of an @racket[if] statement and negate the @emph{condition}.
The design of this refactoring tool further emphasizes the use of syntax properties as a communication channel between
a language and tools that operate on that language and a central feature in the use of Racket as a language workbench.

@section{Variants}
The @racket[if] statement refactoring is valid in MiniJava because MiniJava's syntax requires that each
@racket[if] statement is paired with an @racket[else] clause. Full Java lacks this restriction, thus rendering
our restructuring invalid in the general case. In a language with both kinds of @racket[if] statements, the
refactoring tool must be able to distinguish between the two varieties in order to only allow the restructuring
in places where it preserves the semantics of the original program.

Another possible variant relating to if refactoring includes a conversion between MiniJava expressions using
@racket[&&] and @racket[||]. A transformation like the one we perform on @racket[if] statements could convert
expressions using @racket[&&] into expressions using @racket[||] by applying De Morgan's law.

Finally, it is worth noting that the implementation of our refactoring tool is not MiniJava specific. The refactoring
mainly relies on the presence of a syntax property which can be attached by any language building on top of Racket.
In relatively few lines of code we have enabled our refactoring tool to perform the @racket[if] restructuring on
parenthesized MiniJava and Racket programs as well as MiniJava programs.

@section{Usability}
Within the DrRacket programming environment, the @racket[if] refactoring is available both as a right-click menu option
and bound to a keyboard shortcut. In either case, the refactoring is only available when the current editor position
is within an @racket[if] statement where the refactoring applies. Unless a user explicitly uses the keyboard shortcut or
right-clicks within the DrRacket definitions window, the refactoring tool does not affect the usability of DrRacket or the
MiniJava language.

@section{Impact}
The refactoring tool is largely stand-alone, however, because the refactoring requires information about the object language
to which it applies, the tool's implementation requires minor additions to the object language implementation. Specifically,
in the process of compiling MiniJava to Racket a syntax property must be attached to the syntax objects corresponding to
@racket[if] statements to record information about the source location of the @racket[if] statement's @emph{condition} as well as
the @emph{then} and @emph{else} branches.

@section{Composability}
Our restructuring tool is implemented as a plug-in for DrRacket which allows straightforward composition with other DrRacket plug-ins
and to easily compose with our solutions to the other benchmark problems. In particular the refactoring applies to @racket[if] statements
within the @racket[2dstate-machine] form just as well as programs that do not use the state machine syntax. Refactoring within the
state machine notation, however, does require editing the tabular notation because the introduction of new characters to the editor
will move pieces of the @racket[2d] boxes out of place.

@section{Limitations}
The refactoring tool requires the presence of a syntax property attached to MiniJava's @racket[if] statments. This limitation
implies that each phase of the MiniJava implementation must take care to preserve syntax properties, in particular, the type
checker which produces parenthesized MiniJava programs from the abstract syntax tree must explicitly propograte syntax properties
from the abstract syntax tree to the type checked result.

@section{Uses and Examples}
Several tools built on top of Racket and DrRacket, including Racket's check-syntax utility, are implemented in a similar fashion
using syntax properties and DrRacket's plug-in mechanism in order to show graphical binding information inside of DrRacket.
The recent Language Server Protocol for Microsoft's Visual Studio Code editor provides a similar interface for implementing
refactorings and other tools by sending program representations and responses across a communication channel to generate
various kinds of program information to support refactorings, warnings, or other features.

@section{Effort}
The implementation of the @racket[if] restructuring tool required under 200 lines of code including the small changes made to
the MiniJava implementation to insert the necessary syntax property on @racket[if] statements. Altogether the implementation of
the refactoring tool was completed in approximately one day of work. With an understanding of the DrRacket plug-in system and
general experience with Racket the implementation is straightforward.


@;; Maybe leave this out
@;; @section{Other Comments}

