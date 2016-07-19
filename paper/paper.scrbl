#lang scribble/sigplan @10pt

@(require pict/code
          "mj-examples.rkt"
          scriblib/figure
          (only-in scribble/manual racket racketblock hash-lang)
          (only-in racket/format ~a))

@; NOTES:
@;; Is there anything interesting to discuss about the lexer/parser?
@;; Make sure to list the artifact


@;; TODO: citations
@;;  - MiniJava
@;;  - Racket manifesto
@;;  - You want it when
@;;  - Languages as Libraries
@;;  - Advanced macrology and impl ts
@;;  - syntax-parse
@;;  - typed racket
@;;  - scribble/ algol60/datalog docs
@;;  - syntax parameters
@;;  - sets of scopes?
@;;  - Racket TR1
@;;  - readtables??? docs???
@;;  - dave herman blog post

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
a solution to the benchmark problem Beyond-Grammar Restrictions that extends MiniJava with the @racket[break]
keyword and limits the context in which it is allowed to MiniJava @racket[while] statements. Finally, in
@secref{editing} we consider the problem of program Restructuring in the Editing category by discussing the
implementation of a refactoring tool for MiniJava programs that must be aware of the structure of MiniJava
programs in order to preserve syntactic and semantic correctness.


@;; NOTE: This section should introduce macros and fully expanded code
@section[#:tag "racket-lwc"]{Racket as a Language Workbench}
@;; Sections
@;; - #lang/module-begin talk about the pipeline and typechecking
@;;   show the typechecking module-begin impl
@;; - expand the program to use the even class in a main class (use while and new)
@;; - demonstrate syntactic extension with MiniJava's while macro
@;; - talk about compile time information/define-syntax with the implementation of new
@;; Then the refactoring and break sections can talk about syntax properties and syntax parameters

In this section, we discuss the use of Racket as a language workbench using the implementation
of MiniJava as a guiding example. Additionally, this section provides necessary background for addressing
our solutions to the benchmark problems. We begin with a high-level overview of Racket then discuss many
of its features in depth using our implementation of MiniJava as a driving example.

@subsection{An Overview of Racket}
@;; brief overview of expansion and syntax objects to set up discussion of syntax properties
@;; later on, and the built in forms that we can rely on to implement languages
@;; (such as syntax-parameters which will be discussed later in the evolution section)
Racket is an untyped call-by-value programming language descending from Scheme and Lisp. As a member
of this family of languages, one of Racket's most prominent features is its powerful hygienic macro
system. Racket's macro system is central to the consideration of Racket as a language workbench.
Extensible syntax allows programmers to extend the programming language as necessary to suit
their needs for the task at hand.

In order to support this level of extensibility, Racket programs are processed in two passes. The first,
@emph{read} pass, turns a textual program into a tree of syntax objects. Syntax objects are rich data structures that
combine a symbolic representation of a program with a number of properties. Properties on syntax objects can
include source location information and lexical binding information as well as arbitrary information that
programmers may attach and access using Racket's @racket[syntax-property] procedure.

After a program has been read, the @emph{expansion} pass translates the syntax objects into fully-expanded
syntax objects. Fully-expanded programs resemble an enriches lambda calculus. This pass is essentially a
compilation step that recursively processes syntax turning language extensions into Racket's core forms.
In the process of expansion new opportunities for expansion, or even new macro definitions, which require
further expansion to produce fully-expanded code. In contrast to function evaluation, in which arguments must
be evaluated before a function is applied in an inside-out process, the expansion process, instead, happens outside-in.
Macros expand to produce syntax objects which may contain other macro invocations which must be further expanded
until the result is a fully-expanded syntax object. Essentially, the expander processes the leaves of a syntax object last,
only after they are contained within a otherwise fully-expanded syntax object.

Racket's facilities for language extension and its rich core library allow programmers to implement entirely new
languages on top of Racket. A notable example is Typed Racket, a gradually-typed sister language of Racket, but
these languages do not need to share Racket's syntax or even its semantics as demonstrated by implementations of
Algol 60, Datalog, and Scribble, a language for formatting prose, on top of Racket. In the rest of this section
we demonstrate several of Racket's features for building languages and their use in the implementation of MiniJava.

@subsection{The Structure of MiniJava}
@;; give the high level overview of the diagram and how each piece fits together ...???
@;; compare tradition compiler pipeline with #lang mechanism ...
@;; describe a bit about the #lang mechanism how it allows implementing totally new languages
@;; uses #lang to find the lexer/parser/typechecking ...
@;; compare with TR/scribble/algol60 etc, languages implemented in Racket with totally different
@;; semantics
@;; use the module-begin from the typechecker as an example in the prose
@;; module system, selective exports language is a set of bindings some of which are recognized specifically

@Figure-ref{mj-impl} shows the structure of our MiniJava implementation on top of Racket. Similar to a standard
compiler, there are phases for lexing, parsing, and type checking. The result of the type checking phase is
an untyped, parenthesized version of MiniJava. In place of a traditional code gneration phase, the implementation
relies on Racket's macro system to transform the parenthesized MiniJava classes into method tables implemented
with Racket's vector data type.

@Figure-ref{mj-syntax} shows an example MiniJava program written using Racket's @emph{#lang} mechanism.
The @emph{#lang} form is central to the process of creating new languages in Racket. The syntax consists
of the #lang form followed by the name of a module that defines a language. This mechanism allows the language
to specify its own lexer and parser which may differ from Racket's. In the case of MiniJava the @emph{mini-java}
language specifies a standard Java style lexer and parser which are called on the concrete syntax to produce an
abstract syntax tree. As described in the previous subsection, this abstract syntax tree is the result of the
@emph{read} pass.

Once the MiniJava program has been read to produce an abstract syntax tree, the type checking phase can also be
considered as a language implementation. @Figure-ref{typecheck-mod-beg} shows the definition of @racket[module-begin]
which performs type checking and translates the abstract syntax tree into parenthesized MiniJava. This module exports
the definiton of @racket[module-begin] as @racket[#%module-begin], the @racket[#%module-begin] form is one of Racket's
core forms which is implicitly added around the body of a program during expansion. Replacing Racket's definition of
@racket[#%module-begin] allows for complete control over the expansion of the module. In our type checker, the
@racket[module-begin] for simply calls a function, @racket[typecheck-program], which traverses the abstract syntax
performing type checking and producing the parenthesized form of the MiniJava program. The resulting syntax from the
type checking function is then spliced into the body of Racket's @racket[#%module-begin] allowing Racket's expander
to take over the rest of the compilation pipeline.

@subsection{Language Constructs as Macros}
@(figure*
  "parenthesized-mj-example"
  "The parenthesized version of the MiniJava example"
  parenthesized-mj-example)
The result of type checking the program in @figure-ref{mj-syntax} produces the parenthesized 

@;; while in Java to while in Racket ..
@;; discuss the implementation of the parenthesized while form (without the syntax parameter mess)
@;; this leaves an example to return to later on
@;; reuse racket variable binding or conditional for the MiniJava forms (linguistic reuse)
@subsection{Inter-Macro Communication}
@;; class definitions compiling to define-syntax stuff ..
@;; using define-syntax to store the compile time method table info, etc ...
@;; maybe use the `new` macro as the example here since it is simpler than `send`
@subsection{DrRacket Integration}
@;; tooling/tool-tips with type info ... using syntax properties (screen shot)
@;; more than just communication (with compiler and) between our macros, also communicate with tools
@;; Talk about syntax properties a little bit with the example of
@;; type info attached as tool-tips, this sets up well for the refactoring later


@;; ====================================================================================================


@subsection{The Implementation of MiniJava}
We have chosen MiniJava as the language to extend in our solutions to the benchmark problems.
In this section we describe the implementation of MiniJava in Racket. @Figure-ref{mj-syntax}
shows an example MiniJava program written using Racket's @bold{#lang} mechanism.



@(figure
  "mj-impl"
  "Structure of the MiniJava implemntation"
  @pipeline-diagram)
@(figure*
  "mj-syntax"
  "A sample MiniJava program"
  @mj-simple-example)

@(figure*
  "mj-sexp-example"
  "Parenthesized MiniJava"
  @mj-paren-example)

@Figure-ref{mj-sexp-example} shows the parenthesized version of the MiniJava program from @figure-ref{mj-syntax}.
The parenthesized version of MiniJava programs are the the result of parsing the concrete syntax of MiniJava
and performing type checking over the abstract syntax tree. The type checking pass is necessary to insert type
information where it will be used in the process of macro expansion. For example, consider the method call from
@figure-ref{mj-syntax}
@;; FIXME: Want to use codeblock here to get syntax coloring, but just the method call doesn't parse
@racketblock[this.is_odd(n-1)]
The type checking pass propogates information about the type of @racket[this], in this example @racket[Even],
into the method call resulting in the parenthesized expression:
@racketblock[(send this Even is_odd (- n 1))]
When macro expansion further translates this method call, the implementation of the @racket[send] macro
uses the type information to find the correct index into the method table for the @racket[Even] class.
The type information is necessary here in order to always properly resolve method calls into the correct
corresponding vector references. Since the @racket[send] macro has a reference to the type information,
it can be implemented separately from the @racket[define-class] macro. This form of communication between
different macro implementations is a powerful tool for designing languages and the use of Racket as a
language workbench.

To better understand the macro expansion process and how to implement macros in Racket, consider the use
of the logical or operation in the MiniJava program from @figure-ref{mj-syntax}. In our implementation,
the MiniJava expression @racket[(x || y)] compiles into the Racket expression @racket[(or x y)]. The Racket
definition of the @racket[or] operatation is reproduced below:
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

The previous example defining the @racket[or] macro shows one use of the @racket[define-syntax] form to define
a macro. In general, however, the @racket[define-syntax] form creates a @emph{transformer binding}. Transformer
bindings include macros as in the example defining the @racket[or] macro, but in the general case @racket[define-syntax]
creates bindings whoses values are available at expansion time. This allows the use of the @racket[define-syntax] form
to store static information that can be accessed at compile time using the @racket[syntax-local-value] procedure.

Our implementation of MiniJava expands class forms to bind static information about class types using
@racket[define-syntax]. In @figure-ref{mj-even-static-info}, the class name @racket[Even] is bound to a
record, created with a call to the procedure @racket[static-class-info], containing three elements necessary
@;; Do we need an example of the constructor definition for instances of the even class???
to compile MiniJava classes. @racket[Even:static-method-info], the first element in the record, is a compile-time
table that maps method identifiers to their indices in the method table. The second element, @racket[#'Even:method-table],
is a syntax object that corresponds to the identifier bound as the runtime method table for the @racket[Even] class in
@figure-ref{mj-compiled-vector}. The last element in the record, @racket[#'Even:constructor], is similarly the syntax
object for an identifier bound, at runtime, as a function of no arguments which constructs new instances of the
@racket[Even] class.

All of the preceeding details of Racket's macro system are used in @figure-ref{mj-send-macro} in
the implementation of the @racket[send] macro which implements method calls in MiniJava programs.
Just as in the example of the @racket[or] macro, the @racket[send] macro expects syntax that matches
the pattern for a parenthesized MiniJava method call:
@racketblock[
(send the-class receiver
  method-name arg ...)]
This pattern expects the @racket[send] keyword followed by the name of the class to which the
method belongs, the object on which the method is called, and finally a sequence of zero or
more arguments being passed to the method. The body of the @racket[send] macro acesses the static
information associated with the class name using @racket[syntax-local-value] and extracts the
compile-time method table that maps method names to their indices in the run-time method table.
@;; I think that more should go here, but I don't know what else there is to say about the
@;; MiniJava implementation

The implementation of MiniJava highlights many of Racket's strengths as a language workbench.
The ability to define macros that can query compile-time state in addition to their arguments allows for complex extensions to the language.
Being able to run arbitrary code at compile time allows a number of approaches to language implementation.
One approach, that we have not taken with MiniJava, is to compile from the MiniJava syntax into Racket through a series of
compile-time functions that transform the surface syntax into the vector-based methods tables. Instead we have chosen to rely
on Racket's macro expander as much as possible and make use of compile-time values as a communication channel between different macros.




@;; NOTE: overarching message of trying to use Racket/the macro expansion process
@;; as a source of modularity. Many small, independent extensions that cooperate
@;; to implement MiniJava rather than a monolithic compiler into Racket code



@;; FIXME: Adjust the method table example to show the provide of the runtime class as well
@;;        as compile time information

@;; The mj class form must provide compile time information to other modules
@;; to support compilation of methods calls in other files/classes/etc
@;; using syntax time information as a communication channel

@;; NOTE: syntax properties can be discussed in more detail in the refactoring section
@;; Other communication channels are possible, ie syntax properties
@;; TODO: need an example of syntax-properties somehow ...

@(figure*
  "mj-new"
  "The implementation of new in parenthesized MiniJava"
  @mj-new)

@(figure*
  "typecheck-mod-beg"
  "Typechecking the abstract syntax tree"
  @typecheck-mod-beg)


@(figure*
  "mj-compiled-vector"
  "The Compiled Represenation of MiniJava"
  @mj-compiled-vector)

@(figure
  "mj-even-static-info"
  "Static Information Associated with the Even Class"
  @mj-even-static-info)

@(figure*
  "mj-send-macro"
  "Definition of the MiniJava send form"
  @mj-send-macro)



@include-section{notation.scrbl}
@include-section{evolution.scrbl}
@include-section{editing.scrbl}
@section[#:tag "conclusion"]{Conclusion}
