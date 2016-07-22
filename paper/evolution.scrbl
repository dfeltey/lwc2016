#lang scribble/sigplan @10pt
@(require (prefix-in s: scribble/base)
          (only-in scribble/manual racket racketblock))
@(define (section title)
   (s:section #:tag (string-append "evolution:" title) title))
@title[#:tag "evolution"]{Evolution and Reuse: Beyond-Grammar Restrictions}
This section presents our solution to the @emph{Beyond-Grammar Restrictions} benchmark problem.
Our solution extends MiniJava with a @racket[break] keyword, that is only valid within @racket[while] loops.


@section{Assumptions}
We assume access to the Racket implementation of MiniJava. Although the addition of the @racket[break]
keyword does not involve a significant change to any phase of the implementation it does require small
modifications to each piece of the implementation.

@section{Implementation}

@section{Variants}
One variant of this benchmark problem would be to add Java's @racket[super] keyword to MiniJava.
The @racket[super] keyword, like @racket[break], is only valid in certain contexts, notably in the non-static
methods of classes which inherit from a parent class. The technique of using syntax parameters is well-suited
to restricting such uses of super, in fact the @racket[super] keyword in Racket's class system is implemented
as a syntax parameter. MiniJava's type system, however, would rule out any improper use of @racket[super] before
such an error would be caught by the use of an unmodified syntax parameter, therefore the addition of
@racket[super] to our implementation MiniJava would not be a compelling use of syntax parameters. 

@section{Usability}
Our solution adds @racket[break] as a new statement to the grammar of MiniJava statements, therefore the
@racket[break] keyword is allowed, syntactically, wherever a statement is in MiniJava. Programmers may
use @racket[break] as they see fit, subject to errors only when the keyword appears outside a @racket[while]
loop.

@section{Impact}
Implementing the @racket[break] keyword in MiniJava requires changes to each piece of MiniJava artifact.
To support @racket[break] in the concrete syntax a new lexer token and parsing rule must be added to include
@racket[break] in the abstract syntax tree. A new type checking rule must be added to the MiniJava type checker
to support the new abstract syntax node which simply succeeds and produces the parenthesized MiniJava syntax
corresponding to a @racket[break] statement. Finally, @racket[break] must be added as a syntax parameter in the
implementation of parenthesized MiniJava. The addition of a syntax parameter is a simple addition, however, to
allow the use of @racket[break] within @racket[while] loops the implementation of MiniJava loops must be updated
to modify the @racket[break] syntax parameter. Overall, these changes were mostly independent of other pieces of
the implementation except for the modifications to parenthesized MiniJava's @racket[while] macro.

@section{Composability}
The addition of @racket[break] to MiniJava coexists with our solutions to the two other benchmark problems
without any modification. Our implementation of @racket[break] should compose seamlessly with other instances
of the @emph{Beyond-Grammar Restrictions} problem. Complications would likely only arise from additions which
would modify the behavior of @racket[break] or otherwise alter the behavior of @racket[while] loops. In such a
scenario, however, composability issues would be expected.

@section{Limitations}
This approach to implementing language restrictions relies on a language that compiles to a set of macros implemented
in Racket. The use of syntax parameters necessitates an implementation that relies, in part, on macro expansion. An
alternate implementation of MiniJava that directly produced fully-expanded Racket programs would not be able to take
advantage of syntax parameters as we have done.

@section{Uses and Examples}
Racket uses syntax parameters in many of its core libraries to solve the problem of restricting certain syntactic forms
to only be allowed in specific situations. For example, Racket implements the @racket[this] keyword using syntax parameters
to ensure that it is only valid and expands correctly within Racket's @racket[class] form.

@section{Effort}
Starting from our base implementation of MiniJava, implementing the @racket[break] keyword required fewer than twenty lines
of code and took approximately thirty minutes. The largest changes made were those made to the @racket[while] macro in the
implementation of parenthesized MiniJava.

@;; Maybe leave this out
@;; @section{Other Comments}

