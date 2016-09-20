#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          (only-in scribble/manual racket racketblock)
          scriblib/figure
          "bib.rkt"
          "mj-examples.rkt")

@(figure*
  "break-impl"
  "The break syntax parameter and its use in while"
  break-impl)

@(define (section title)
   (s:section #:tag (string-append "evolution:" title) title))
@title[#:tag "evolution"]{Evolution and Reuse: Beyond-Grammar Restrictions}
This section presents our solution to the @emph{Beyond-Grammar Restrictions} benchmark problem in the @emph{Evolution} category.
Our solution extends MiniJava with a @racket[break] keyword that is valid only within @racket[while] loops.
The implementation uses Racket's @emph{syntax parameters} to control the meaning of a
binding depending on its context@~cite[syntax-params].

@section{Assumptions}
Our implementation technique for @racket[break] relies on our use of macros to
implement MiniJava constructs, as syntax parameters interact with the expansion
process.

@section{Implementation}
Syntax parameters are syntax bindings whose expansion can be controlled by
macros in their context.
Specifically, macros (such as @racket[while]) may adjust the meaning of syntax
parameters (such as @racket[break]) to make the latter behave as if they had
been lexically bound by the former. This is akin to the way lexical scoping lets
programmers adjust the meaning of an identifier in some context by introducing
a shadowing binding for it.

@Figure-ref{break-impl} shows the definition of @racket[break], as well as an
extended version of @racket[while] which cooperates with it.
Originally, @racket[break] is bound to a transformer which always raises a
syntax error, which statically rules out uses of @racket[break] outside of
@racket[while].
The definition of @racket[while] adjusts the meaning of @racket[break] (using
@racket[syntax-parameterize]) within its body to instead call an escape
continuation and break out of the loop.

@section{Variants}
One variant of this benchmark problem would be to add Java's @racket[super] keyword to MiniJava.
The @racket[super] keyword, like @racket[break], is valid only in certain
contexts---in methods of child classes, specifically.

The current implementation always breaks the nearest enclosing loop, but @racket[break]
could accept a (literal) number as a argument to allow breaking of nested loops.

@; Another possible variant is the implementation of named @racket[break] statements.
@; This would require the addition of a macro to support labels and a way to jump from the expansion of a @racket[break]
@; keyword to a named label.
 
@section{Usability}
Our addition of the @racket[break] keyword to MiniJava is syntactically allowed wherever a statement is valid.
The syntax parameter mechanism, however, disallows uses of @racket[break]
outside the body of @racket[while] loops, as one would expect.

@section{Impact}
Beyond the changes to parenthesized MiniJava discussed previously, implementing @racket[break] requires
changes to our MiniJava lexer, parser, and type checker.
We extend the lexer to recognize the @racket[break] keyword and produce a corresponding token.
Similarly, we modify the parser to produce abstract syntax representing a use of @racket[break].
The type checking rule added for @racket[break] always succeeds and produces a use of the @racket[break]
syntax parameter in parenthesized MiniJava. Overall, these changes are small and independent of the rest of
our MiniJava implementation.

@section{Composability}
Our addition of @racket[break] to MiniJava integrates seamlessly with our solutions to the other two benchmark problems.
This implementation would compose well with other instances of the @emph{Beyond-Grammar Restrictions} problem.
@; with the possible exception of other extensions which also modify the MiniJava @racket[while] macro.

@section{Limitations}
Implementing language restrictions using syntax parameters necessitates a language that compiles via a set of macros.
An alternate implementation of MiniJava that directly produced fully-expanded Racket programs would not be able to
use this strategy. Our use of syntax parameters relies on the nested shape of Racket's syntax.
The @racket[syntax-parameterize] form only adjusts the meaning of forms nested inside of it.

@section{Uses and Examples}
Racket uses syntax parameters in many of its core libraries to restrict certain syntactic forms to specific contexts.
For example, Racket implements the @racket[this] keyword using syntax parameters to ensure that it is valid only within Racket's @racket[class] form.

@section{Effort}
Extending our implementation of MiniJava to support the @racket[break] keyword requires fewer than twenty lines of
code. The implementation took approximately thirty minutes, but required familiarity with our MiniJava implementation
and the use of syntax parameters.
