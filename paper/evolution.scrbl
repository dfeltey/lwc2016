#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          (only-in scribble/manual racket racketblock)
          scriblib/figure
          "mj-examples.rkt")
@(define (section title)
   (s:section #:tag (string-append "evolution:" title) title))
@title[#:tag "evolution"]{Evolution and Reuse: Beyond-Grammar Restrictions}
This section presents our solution to the @emph{Beyond-Grammar Restrictions} benchmark problem in the @emph{Evolution} category.
Our solution extends MiniJava with a @racket[break] keyword, that is only valid within @racket[while] loops.
In the implementation, we use Racket's @emph{syntax parameters} which allow control over the meaning of a
binding depending on its context.

@section{Assumptions}
Our implementation technique for @racket[break] relies on our MiniJava implementation using macros for its compilation.

@section{Implementation}
@(figure*
  "break-impl"
  "The break syntax parameter and its use in while"
  break-impl)
@Figure-ref{break-impl} shows the definition of the @racket[break] keyword as a syntax parameter and the
extension to MiniJava's @racket[while] macro to support @racket[break].
Syntax parameters allow programmers to selectively modify the meaning of a binding for the extent of macro expansion.
The @racket[break] keyword is bound as a syntax parameter to a transformer which always raises a syntax error.
This binding guarantees that using @racket[break] outside of a @racket[while] loop is statically invalid.

We extend MiniJava's @racket[while] macros to support the @racket[break] keyword using @racket[syntax-parameterize]
to change the meaning of @racket[break] inside a @racket[while] loop's body.
Specifically, uses of @racket[break] will expand to call the escape-continutation bound by Racket's @racket[let/ec]
which will terminate the loop if called.

@section{Variants}
One variant of this benchmark problem would be to add Java's @racket[super] keyword to MiniJava.
The @racket[super] keyword, like @racket[break], is only valid in certain contexts.
Specifically, @racket[super] is valid only in non-static methods of classes which inherit from a parent class.

Another possible variant is the implementation of named @racket[break] statements.
This would require the addition of a macro to support labels and a way to jump from the expansion of a @racket[break]
keyword to a named label.
 
@section{Usability}
Our addition of the @racket[break] keyword to MiniJava is syntactically allowed whereever a statement is valid.
The syntax parameter mechanism, however, disallows uses of @racket[brea] outside the body of @racket[while] loops.

@section{Impact}
Beyond the changes to parenthesized MiniJava discussed previously, implementing @racket[break] requires
changes to the the MiniJava lexer, parser, and type checker.
We extend the lexer to recognize the @racket[break] keyword and produce a corresponding token.
Similarly, we modify the parser to produce abstract syntax representing a use of @racket[break].
The type checking rule added for @racket[break] always succeeds and produces a use of the @racket[break]
syntax parameter in parenthesized MiniJava. Overall, these changes are small and independent of the rest of
our MiniJava implementaion.

@section{Composability}
Our addition of @racket[break] to MiniJava integrates seamlessly with our solutions to the other two benchmark problems.
This implementation would compose well with other instances of the @emph{Beyond-Grammar Restrictions} problem
with the possible exception of other extensions which also modify the MiniJava @racket[while] macro.

@section{Limitations}
Implementing language restrictions using syntax parameters relies on a language that compiles to a set of macros implemented
in Racket. An alternate implementation of MiniJava that directly produced fully-expanded Racket programs would not be able to
use this strategy.

@section{Uses and Examples}
Racket uses syntax parameters in many of its core libraries to restrict certain syntactic forms to specific contexts.
For example, Racket implements the @racket[this] keyword using syntax parameters to ensure that it is only valid within Racket's @racket[class] form.

@section{Effort}
Extending our implementation of MiniJava to support the @racket[break] keyword requires fewer than twenty lines of
code. The implementation tooks approximately thirty minutes assuming a familiarity with our MiniJava implementation
and the use of syntax parameters.
