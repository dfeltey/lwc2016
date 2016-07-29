#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          (only-in scribble/manual racket racketblock)
          scriblib/figure
          "mj-examples.rkt"
          "bib.rkt")
@(define (section title)
   (s:section #:tag (string-append "editing:" title) title))
@title[#:tag "editing"]{Editing: Restructuring}
From the @emph{Editing} category we tackled the @emph{Restructuring} benchmark problem.
Specifically, we built a refactoring tool for MiniJava which restructures @racket[if]
statements by swapping the @emph{then} and @emph{else} branches and negating the condition. 

@(figure*
  "refactor-use"
  "Using the refactoring tool in DrRacket"
  refactoring-pict)

@section{Assumptions}
We assume that we can modify the implementation of MiniJava to expose additional information
about conditional statements. 

@section{Implementation}
@(figure
  "refactor-prop"
  "The syntax property used to implement the refactoring tool"
  refactor-impl)

Our restructuring tool relies on source location information for conditionals, which it gets from our MiniJava implementation.
To do this, we extended MiniJava's @racket[if] macro to attach a syntax property mapping
the key @racket['refactor] to a list containing the source position and span of each each piece of the @racket[if]
statement: the @emph{condition}, the @emph{then} branch, and the @emph{else} branch.
@Figure-ref{refactor-prop} shows this extension.

We implement our tool as a plugin for DrRacket@~cite[drracket] which processes a program's fully-expanded syntax to find syntax objects with the @racket['refactor] syntax property attached.
These locations are where the refactoring may apply. When a user applies the refactoring, the tool rewrites the conditional within the editor's buffer.

@section{Variants}
A variant on this refactoring would be to transform expressions that use @racket[&&] into expressions that use @racket[||] by applying De Morgan's law.

In addition, it is worth noting that the implementation of our refactoring tool is not MiniJava specific.
By parameterizing the refactoring rule over negation syntax, the tool generalizes across languages.
To support the refactoring, a language simply needs to attach the relevant syntax property to its conditional form.
As a proof of concept, we also extend Racket's @racket[if] in this fashion.

@section{Usability}
As @figure-ref{refactor-use} shows, the @racket[if] refactoring is accessed by right-clicking inside of an @racket[if] statement
or using a keyboard shortcut. The refactoring tool does not affect the usability of DrRacket or the
MiniJava language otherwise.

@section{Impact}
The refactoring tool proper is a standalone piece of code.
Otherwise the only changes to our MiniJava implementation were to the @racket[if] macro as discussed earlier.

@section{Composability}
Our restructuring tool composes automatically with other DrRacket plug-ins.
Additionally, it does not interfere with our solutions to the other benchmark problems.
The only caveat, however, is that refactoring within the state-machine notation may break the alignment of the
tabular syntax.

@section{Limitations}
This refactoring only makes sense for conditionals with exactly two branches. In a language like full Java, which also
has single-branch @racket[if] statements, the tool would have to distinguish these cases to determine where the refactoring applies.

@section{Uses and Examples}
Several tools built on top of Racket and DrRacket use syntax properties to facilitate communication between tools and language implementations.
Specific examples include Racket's check-syntax utility, Typed Racket's type tool-tips, and Racket's feature-specific profiler@~cite[fsp].

@section{Effort}
The implementation of the @racket[if] restructuring tool requires under 200 lines of code, including the small changes made to
the MiniJava implementation. The implementation took approximately one day, assuming a basic understanding of the DrRacket plug-in system.

