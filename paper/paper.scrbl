#lang scribble/sigplan @10pt

@(require pict/code
          "mj-examples.rkt"
          scriblib/figure
          (only-in scribble/manual racket racketblock))

@; NOTES:
@;; Is there anything interesting to discuss about the lexer/parser?
@;; Make sure to list the artifact


@;; TODO: citations
@;;  - MiniJava
@;;  - Racket manifesto
@;;  - You want it when
@;;  - Languages as Libraries
@;;  - Racket class system??
@;;  - syntax-parse

@title{Language Workbench Challenge 2016: Racket Submission}

@abstract{
We present a submission to the 2016 Language Workbench Challenge using
the Racket programming language. 
@;; TODO: Fill in the rest of these details later          
}

@section[#:tag "intro"]{Introduction}
This paper demonstrates the use of the Racket programming language and the DrRacket programming
environment as a language workbench. For the 2016 Language Workbench Challenge, we have
chosen MiniJava as the object language which we extend in the implementations of three benchmark
problems.

In @secref{racket-lwc} we introduce many of the features that make Racket suitable for use as
a language workbench, using the implementation of MiniJava as a driving example. After this
introduction to Racket we present our solutions to three benchmark problems of the 2016 Language
Workbench Competition. In @secref{notation} under the Notation category we discuss our solution to
the Tabular Notation problem by introducing an exntesion to MiniJava that supports a tabular notation
for specifying state machines. @Secref{evolution} addresses the category of Evolution and Reuse, offering
a solution to the benchmark problem Beyond-Grammar Restrictions that extends MiniJava with the @racket[super]
keyword and limits the contexts in which it may be used. Finally, in @secref{editing} we consider the
problem of program Restructuring in the Editing category by discussing the implementation of a refactoring
tool for MiniJava programs that must be aware of the structure of MiniJava programs in order to preserve
syntactic and semantic correctness.




@;; NOTE: This section should introduce macros and fully expanded code
@section[#:tag "racket-lwc"]{Racket as a Language Workbench}
In this section, we discuss the use of Racket as a language workbench using the implementation
of MiniJava as a guiding example. Additionally, this section provides necessary background for addressing
our solutions to the benchmark problems.

Racket, the programming language, is an untyped call-by-value programming language descending from Scheme and
Lisp. As a member of this family of languages, one of Racket's most prominent features is its powerful
hygienic macro system. Racket's macro system is central to the consideration of Racket as a language
workbench. Extensible syntax allows programmers to extend the programming language as necessary to suit
their needs for the task at hand.

@;; FIXME: Bad transition, how to bridge macros and expanded programs ?

Racket programs are processed in two passes. The first, read pass, turns a textual program into
a syntax object. Syntax objects combine a symbolic s-expression representation of a program along
with information about source locations and lexical binding among others. After a program has been
read, an expansion pass further processes the syntax object to produce a fully-expanded syntax object.
Fully-expanded programs resemble an enriched lambda calculus. Because programmers may extend the language
as they see fit with new macro definitions, the expansion process is necessarily recursive and may introduce
new opportunities for expansion, or even new macro definitions during the traversal of a syntax object.

@;; FIXME: is there a better example for this?
@(figure*
  "mj-example"
  "The Syntax of MiniJava"
  @mj-simple-example)

@(figure*
  "mj-sexp-example"
  "Parenthesized MiniJava"
  @mj-paren-example)
  

@;; This section discusses the use of macros syntax-classes to implement mini-java
We have chosen MiniJava as the language to extend in our solutions to the benchmark problems.
In this section we describe the implementation of MiniJava in Racket. @Figure-ref{mj-example}
shows an example MiniJava program written using Racket's @bold{#lang} mechanism.

There are two main components of our MiniJava implementation. The first is a standard Java lexer
and parser to support the usual concrete syntax of MiniJava. The second component is the implementation
of an prefix s-expression based version of MiniJava which is produced by our parser. @Figure-ref{mj-sexp-example}
shows the prefix variant of the MiniJava program that our parser produces from the program in @figure-ref{mj-example}.
The parenthesized MiniJava is implemented as a number macros that transform the parenthesized MiniJava forms into
method tables implemented as Racket vectors.

The parenthesized version of MiniJava programs are the the result of parsing the concrete syntax of MiniJava
and performing type checking over the abstract syntax tree. The type checking pass is necessary to insert type
information where it will be used in the process of macro expansion. For example, consider the method call
@racketblock[this.is_odd(n-1)]
The type checking pass propogates information about the type of @racket[this], in this example @racket[Even],
into the method call resulting in the expression:
@racketblock[(send this Even is_odd (- n 1))]
When macro expansion further translates this method call to look up the correct indices in the method table
for the @racket[Even] class the type information is required in order to correctly resolve the method.
@;; FIXME: is this actually true?????
Since the @racket[send] macro has a reference to the type information, it can be implemented separately
from the @racket[define-class] macro. This form of communication between different macro implementations
is a powerful tool for designing languages and the use of Racket as a language workbench.


@;; FIXME: better transition into details of macros/implementation ...
@;; this next "paragraph" needs to be broken up somehow
@;; start with the small example of the or macro on the inside of the m-j program
@;; then build out towards a description of the mini-java compilation process
In contrast to function calls which must first evaluate their arguments before the function may be applied
in an inside-out process, the process of macro expansion, instead, happens outside-in. First outer macros are
expanded and then any new unexpanded code that results from the first round of expansion will be
expanded until the program is fully expanded. In order to better understand the macro expansion process and
how to implement macros in Racket, consider the use of the logical or operation in the
MiniJava program from @figure-ref{mj-example}. In our implementation, the MiniJava expression
@racket[(x || y)] compiles into the Racket expression @racket[(or x y)]. The Racket definition of the
@racket[or] operatation is reproduced below:
@racketblock[
 (define-syntax (or stx)
  (syntax-parse stx
    [(or x y)
     #'(let ([v x])
         (if v v y))]))]
This definition introduces @racket[or] as a macro which transforms expressions of the form @racket[(or x y)]
into the equivalent let-expression. The short-circuiting behavior of Java's logical or operation necessitates
the implementation as a macro. If @racket[or] were defined as a function instead then both arguments to the
function would be evaluated before either is checked for truthfulness which could lead to spurious errors in
MiniJava programs. The @racket[syntax-parse] form allows the definition of a macro via pattern matching.
A use of @racket[syntax-parse] may have any number of clauses that contain a pattern and a result. In the
above example the pattern is @racket[(or x y)], this includes the identifier @racket[or] because @racket[syntax-parse]
will receive the entire syntax object corresponding to @racket[(or x y)]. The variables, @racket[x] and @racket[y],
used in the pattern indicate that they can match any piece of syntax. The pattern matching mechanism binds @racket[x]
and @racket[y] for use in syntax templates, like the one that occurs following the pattern. The @racket[#'(let ...)]
notation in this example demonstrates the construction of a syntax object. The expression @racket[#'expr] is equivalent
to @racket[(syntax expr)] and is similar to the @racket[quote] form that Racket inherits from Lisp. The key difference
between @racket[quote] and @racket[syntax] is that the latter produces a syntax object rather than a datum.
Syntax objects are similar to s-expressions in that they contain an s-expression based representation of
syntax, but they also contain other information such as source location and details about lexical binding structure.
Additionally, programmers may attach other useful information to syntax objects as syntax properties. For example,
Racket's expansion process adds an @emph{origin} property to expanded syntax that tracks the series of macros
that expand to produce a given syntax object.

Beyond simple pattern based macros such as the example of @racket[or] above, Racket allows the use of the entire
programming language to implement new syntactic constructions. This means that arbitrary code, including
side-effecting code, may need to run at compile time in order to expand a syntactic form. Racket addresses
the issue of mingling compile time code with run-time code through a phase distinction that makes explicit
the execution time of a piece of code.


A more complex example of the macro expansion process used in our implementation of MiniJava is the translation
from method calls to instance and method table lookups. Consider the compilation from the parenthesized MiniJava
in @figure-ref{mj-sexp-example} into the method table in @figure-ref{mj-compiled-vector}. The method call in the
definition of @racket[is_even]
@racketblock[(send this Even is_odd (- n 1))]
compiles into the Racket expression:
@racketblock[
 ((vector-ref
   (vector-ref this 0) 1)
  (- n 1))]
The inner use of @racket[vector-ref] looks up the class method table of the @racket[this] object which is
stored at index 0. The outer @racket[vector-ref] is an index into the method table of the @racket[Even] class.
Since the @racket[is_odd] method is the second method of the class it is stored at index 1, this reference returns
a function which is called on the argument to the method.


To support this compilation, the @racket[send] macro must have access to information about the @racket[Even]
class. In particular, the @racket[send] macro needs to determine the index of the @racket[is_odd] method in
the method table. Additionally, keeping the definition of the @racket[send] form independent of the the
@racket[define-class] form is preferable for a modular language design. In order to satisy all of these
constraints it is necessary that the MiniJava parser inserts the class of the receiver of a method call into
the @racket[send] expression, and that static information about methods is bound to the name of the class.
@Figure-ref{mj-even-static-info} shows the static information bound to the @racket[Even] class using Racket's
@racket[define-syntax] form.



@;{
To support this compilation, the implementation of the MiniJava @racket[class] form must compute information
about the order of the methods in the class before expanding the methods in order to generate the correct
offsets. This compile-time information is necessary, more generally, to any class which may instantiate the
@racket[Even] class and call its methods. In order to convey the information about the indices of methods,
the compilation of the @racket[Even] class must produce and provide compile-time data that can be used in
the compilation of other classes to resolve method calls and field accesses.
}

@;; FIXME: Adjust the method table example to show the provide of the runtime class as well
@;;        as compile time information

@;; The mj class form must provide compile time information to other modules
@;; to support compilation of methods calls in other files/classes/etc
@;; using syntax time information as a communication channel

@;; NOTE: syntax properties can be discussed in more detail in the refactoring section
@;; Other communication channels are possible, ie syntax properties
@;; TODO: need an example of syntax-properties somehow ...




@;; Some of this may still be relevant, but need to build up to it
@;{
The lexer and parser are a standard Lex/Yacc style lexer and parser, so we will focus on the compilation
from the parenthesized MiniJava into Racket. The macro that implements the MiniJava class form is reproduced
in @figure-ref{mj-class-macro}. This syntax extension uses @emph{syntax-parse}, one of many useful utilities
for performing pattern matching over syntax objects in Racket. This macro expects syntax that matches the
shape of a parenthesized MiniJava class, the keyword @emph{class} followed by a name for class and optionally
the name of a superclass followed by a bracketed list of variable and method declarations. The colon syntax used
in syntax-parse, as in @racket[var:var-declaration], specifies that the pattern variable, @emph{var},
should only match syntax belonging to the syntax class @emph{var-declaration}.

Syntax classes give programmers a general way of specifying complex syntax patterns. Additionally,
syntax classes allow programmers to attach arbitrary data to the syntax matched by a syntax class using
attributes. In @figure-ref{mj-class-macro}, the "dot" notation used, as in @racket[var.compiled], accesses
the @emph{compiled} attribute that has been associated with @emph{var} by @racket[syntax-parse].

@Figure-ref{mj-syntax-class} shows a portion the syntax class that corresponds to MiniJava statements.
A piece of syntax matches the statement syntax class when it matches any of the declared patterns.
The @racket[#:with] declaration within each pattern associates data with any syntax that matches the
syntax class. In this example, the @emph{compiled} attribute is the 
}

@(figure*
  "mj-compiled-vector"
  "The Compiled Represenation of MiniJava"
  @mj-compiled-vector)

@(figure*
  "mj-even-static-info"
  "Static Information Associated with the Even Class"
  @mj-even-static-info)

@(figure*
  "mj-syntax-class"
  "Syntax Class for MiniJava Statements"
  @mj-statement-syntax-class)

@(figure*
  "2d-state-machine"
  "Tabular Notation for State Machines"
  @2d-state-machine)

@section[#:tag "notation"]{Notation: Tabular Notation}
@include-section{notation.scrbl}
@section[#:tag "evolution"]{Evolution and Reuse: Beyond-Grammar Restrictions}
@include-section{evolution.scrbl}
@section[#:tag "editing"]{Editing: Restructuring}
@include-section{editing.scrbl}
@section[#:tag "conclusion"]{Conclusion}