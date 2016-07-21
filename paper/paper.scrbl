#lang scribble/sigplan @10pt

@(require pict/code
          (only-in pict bitmap scale)
          "bib.rkt"
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

@authorinfo["Daniel Feltey" "Northwestern University" "daniel.feltey@eecs.northwestern.edu"]
@authorinfo["Spencer Florence" "Northwestern University" "spencer.florence@eecs.northwestern.edu"]
@authorinfo["Vincent St-Amour" "Northwestern University" "stamourv@eecs.northwestern.edu"]
@authorinfo["Tim Knutson" "University of Utah" "tkkemo@gmail.com"]
@authorinfo["Ryan Culpepper" "Northeastern University" "ryanc@ccs.neu.edu"]
@authorinfo["Matthew Flatt" "University of Utah" "mflatt@cs.utah.edu"]
@authorinfo["Robert Bruce Findler" "Northwestern University" "robby@eecs.northwestern.edu"]
@authorinfo["Matthias Felleisen" "Northeastern University" "matthias@ccs.neu.edu"]


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

@subsection{An Overview of Racket and its Macro System}
@;; brief overview of expansion and syntax objects to set up discussion of syntax properties
@;; later on, and the built in forms that we can rely on to implement languages
@;; (such as syntax-parameters which will be discussed later in the evolution section)
Racket is an untyped, call-by-value programming language in
the Lisp family. One of Racket's most prominent features is
its powerful hygienic macro system and it is central to the
consideration of Racket as a language workbench. While the
macro system is clearly an evolution of the early
Assembly@~cite[mcilroy] and Lisp@~cite[lisp-macros] macro systems,
it has changed so much that it is nearly unrecognizable as a
descendant of them. Instead, it is easier to approach the
Racket macro system as if it is a domain-specific language
for writing an extensible compiler. Racket's macro
programmers start from the compiler from some initial
language and add new constructs to it, writing declarations
that say how the new constructs compile into existing ones;
perhaps existing cases in the initial language, but also
perhaps constructs in yet some other language. As an example,
Racket has a hierarchy of feature-poor (but error-message
rich) teaching languages that are built using macros on top
of the basic Racket programming language. These languages
consist of macros that compile into regular Racket, but
using scoping declarations provided by Racket's module
system to completely hide access to the more powerful
constructs in Racket proper.

In order to support this level of extensibility, Racket programs are processed in two passes. The first,
@emph{read} pass, turns a textual program into a tree of syntax objects. Syntax objects are rich data structures that
combine a symbolic representation of a program with a number of properties. Properties on syntax objects can
include source location information and lexical binding information as well as arbitrary information that
programmers may attach and access using Racket's @racket[syntax-property] procedure.

After a program has been read, the @emph{expansion} process invokes macros to translate the syntax objects into a
low-level programming language that looks like a lambda calculus with conditionals, local binding forms, and
many primitive functions. In contrast to function evaluation, in which arguments must
be evaluated before a function is applied, in an inside-out process, the expansion process happens outside-in.
The macro expander finds the outermost expression that is not in the core form and applies one step of macro expansion
to it, step by step, eliminating the use of macros from the program.

Racket's facilities for language extension and its rich core library allow programmers to implement entirely new
languages on top of Racket. A notable example is Typed Racket, a gradually-typed sister language of Racket, but
these languages do not need to share Racket's syntax or even its semantics as demonstrated by implementations of
Algol 60, Datalog, and Scribble, a language for formatting prose, on top of Racket. The implementations of these
languages are made possible by Racket's expressive syntax system along with many useful features of the Racket module
system.

In the rest of this section we demonstrate several of Racket's features for building languages and their use in the
implementation of MiniJava.



@subsection{The Structure of MiniJava}
@(figure
  "mj-impl"
  "Structure of the MiniJava implemntation"
  @pipeline-diagram)
@(figure*
  "mj-syntax"
  "A sample MiniJava program"
  @mj-simple-example)
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
an untyped, parenthesized version of MiniJava. In place of a traditional code generation phase, the implementation
relies on Racket's macro system to transform the parenthesized MiniJava classes into method tables implemented
with Racket's vector data type.

@Figure-ref{mj-syntax} shows an example MiniJava program written using Racket's @emph{#lang} mechanism.
The @emph{#lang} form is central to the process of creating new languages in Racket. The syntax consists
of the #lang form followed by the name of a module that defines a language. This mechanism allows the language
to specify its own lexer and parser which may differ from Racket's. In the case of MiniJava the @emph{mini-java}
language specifies a standard Java style lexer and parser which are called on the concrete syntax to produce an
abstract syntax tree. As described in the previous subsection, this abstract syntax tree is the result of the
@emph{read} pass.

@(figure*
  "typecheck-mod-beg"
  "Typechecking the abstract syntax tree"
  @typecheck-mod-beg)

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

@(figure*
  "parenthesized-mj-example"
  "The parenthesized version of the MiniJava example"
  parenthesized-mj-example)
@subsection{Language Constructs as Macros}
The result of lexing, parsing, and type checking the program in @figure-ref{mj-syntax} produces the parenthesized
MiniJava program in @figure-ref{parenthesized-mj-example}. Programs in the parenthesized MiniJava language have a
similar structure to those written in MiniJava's concrete syntax. The two languages share the same basic operators
with the key differences being that the syntax of parenthesized MiniJava is fully prefixed and type information has
been stripped away except for method calls using the @racket[send] form which have been annotated with type information.

Although the parenthesized MiniJava language appears more similar to Racket than MiniJava does, its semantics is still
that of MiniJava. Consider the @racket[while] construct used in @figure-ref["mj-syntax" "parenthesized-mj-example"].
Racket does not have a @racket[while] form, therefore to support this feature in MiniJava we must implement it ourselves.
Because the body of a @racket[while] statement may never run, it cannot be directly implemented as a function since function
calls must evaluate their arguments. Instead we have implemented @racket[while] as a macro.

The implementation of the @racket[while] form is shown in @figure-ref{mj-while-macro}. The definition of @racket[while]
uses @racket[syntax-parse] which provides a powerful mechanism for defining new syntactic extensions via pattern matching.
A use of @racket[syntax-parse] may have any number of clauses that contain a pattern to be matched followed by a series of
definitions and expressions that are evaluated when the pattern matches. The definition of @racket[while] contains a single
patter to match, @racket[(while test body ...)], which indicates that the @racket[while] macro expects to see the identifier
@racket[while] followed by a test expression and any number of body expressions. The pattern matching mechanism binds
@racket[test] and @racket[body] for use within syntax templates, the expression contained within @racket[#'(...)].
The expression @racket[#'expr] is equivalent to @racket[(syntax expr)] and is similar to the @racket[quote] form that Racket
inherits from Lisp. The key difference between @racket[quote] and @racket[syntax] is that the latter produces a syntax object
rather than a datum and supports interpolation as in the @racket[while] definition to support copying the sequence of body
expressions from the pattern to the template.

The @racket[while] macro is a good example of the typical kind of linguistic reuse found in the implementation of many
Racket macros. The @racket[while] forms relies on Racket's built in conditional expressions, @racket[when], and Racket's
@emph{named let} form in order to implement MiniJava's @racket[while] statement. Furthermore, this example shows the
distinction between Racket's run-time and compile-time phases. The local definition of @racket[temp] within the definition
of @racket[while] must happen at compile time since its result, the syntax object of a fresh identifier, is inserted into the
syntax that the @racket[while] macro generates. This means that arbitrary code, including
side-effecting code, may need to run at compile time in order to expand a syntactic form. Racket addresses
the issue of mingling compile time code with run-time code through a phase distinction that makes explicit
the execution time of a piece of code. This is necessary in order to support a tower of linguistic abstractions
that can easily build upon one another to create new languages.

@;; while in Java to while in Racket ..
@;; discuss the implementation of the parenthesized while form (without the syntax parameter mess)
@;; this leaves an example to return to later on
@;; reuse racket variable binding or conditional for the MiniJava forms (linguistic reuse)
@subsection{Inter-Macro Communication}
After a parenthesized MiniJava program is produced, Racket's expander takes over and expands class definitions.
For example, the @racket[Parity] class defined in @figure-ref{parenthesized-mj-example} compiles into the Racket
code in @figure-ref{mj-parity-compiled}. When the @racket[define-class] form expands it produces
three definitions. The first definition is the run-time method table which  is shared by all instances of
the @racket[Parity] class. The second definition is the constructor that creates instances of the @racket[Parity]
class. Finally, the third definition is bound using Racket's @racket[define-syntax] form to static information
about the @racket[Parity] class.

In the previous subsection, we used Racket's @racket[define-syntax] form to define a new macro that implements
MiniJava's @racket[while] statement. In general, however, the @racket[define-syntax] form creates a @emph{transformer}
binding. Transformer bindings include macros, as in the definition of @racket[while], but in the general case
@racket[define-syntax] creates bindings whose values are available at expansion time. This allows the use of the
@racket[define-syntax] form to store static information that can be accessed at compile time using the @racket[syntax-local-value]
procedure. This allows for macros to expand into definitions using @racket[define-syntax] which can be used as a communication
channel between distinct macros that can be consulted to determine their expansion.

@Figure-ref{mj-new} gives an example of a macro which consults static information in order to expand. In this case
the @racket[new] macro accesses the static information bound to the class identifier passed in and looks up the constructor
field of the @racket[static-class-info] record. For example, consider the expression @racket[(new Parity)] from
@figure-ref{parenthesized-mj-example}. The @racket[new] macro receives the identifer @racket[#'Parity]
and calls @racket[syntax-local-value] on it to access the static information in the @racket[static-class-info] 
record bound to the @racket[Parity] identifier then expands into a call to the @racket[Parity:constructor] function
to create a new instance of the class.

Another use of this static information occurs in the @racket[send] macro. The typed elaboration of a MiniJava program
produces by our lexer, parser, and type checker annotates @racket[send] method calls with the name of the class to which 
the method belongs. In this case the @racket[send] macro uses the static information to resolve method calls into
the correct indices in the class's method table. @Figure-ref{parenthesized-mj-example} shows the result of this
expansion within the functions stored in @racket[Parity:method-table].

@;; class definitions compiling to define-syntax stuff ..
@;; using define-syntax to store the compile time method table info, etc ...
@;; maybe use the `new` macro as the example here since it is simpler than `send`
@subsection{DrRacket Integration}
@;; tooling/tool-tips with type info ... using syntax properties (screen shot)
@;; more than just communication (with compiler and) between our macros, also communicate with tools
@;; Talk about syntax properties a little bit with the example of
@;; type info attached as tool-tips, this sets up well for the refactoring later
The use of Racket's @racket[define-syntax] form allows a communication channel between macros, but Racket's
syntax system, more generally, allows communication between languages and external libraries through the
use of @emph{syntax properties}. As discussed above, Racket's syntax objects are a rich data structure that
store more than just a symbolic representation of a program fragment. Syntax objects also contain lexical binding and
source information. Furthermore programmers can attach arbitrary data to syntax objects using the @racket[syntax-property]
function.

@Figure-ref{add-tool-tips} shows a use of syntax properties within the MiniJava type checker used to attach
tool tips containing type information to MiniJava expressions. @Figure-ref{tool-tips} displays a MiniJava program
in the DrRacket integrated development environment displaying the a type tool tip for the @racket[check] identifier.
The mouse-over tool tips are integrated in DrRacket using the syntax property mechanism, DrRacket traverses the fully-expanded
program and determines where tool-tips belong based on the @racket['mouse-over-tooltips] syntax property used in
@figure-ref{add-tool-tips}.

Whereas @racket[define-syntax] allows for communication and cooperation between macros, syntax properties allow language
implementors to collaborate with external tools and libraries that may process or otherwise analyze programs the compile
into Racket. Tool tips are one such feature that show off the usefulness of syntax properties. Additionally, DrRacket's
@emph{check syntax} tool, which draws the arrows between identifiers in @figure-ref{tool-tips}, uses syntax properties
to track uses and binding positions of variables in a source program which may not appear in the fully-expanded syntax
object. As a concrete example, because fields in a MiniJava class are compiled away into vector offsets, a syntax property
with the key @racket['disappeared-binding] is used to record that a binding is present in the source syntax but has been
compiled away in the fully-expanded program.

@subsection{The Racket Language Workbench}
The previous subsections highlight many features of Racket that make it suitable for use as a language workbench.
An extensible language in which programmers can freely add new language forms gives programmers the power to express complex
problems in a domain specific fashion. The ability to define macros that can query compile-time state in addition to
their arguments allows for complex extensions to the language and one powerful communication channel for implementing
cooperating macros. Access to the full Racket programming language at both compile-time and run-time allows for expressive
extensions to the compiler that do not limit the programmer. Being able to run arbitrary code at compile time allows a
number of approaches to language implementation. One approach, that we have not taken with MiniJava, is to compile from
the MiniJava syntax into Racket through a series of compile-time functions that transform the surface syntax into the
vector-based methods tables. Instead we have chosen to rely on Racket's macro expander as much as possible and make use of
compile-time values as a communication channel between different macros. Finally, syntax objects and their attached syntax
properties allow for yet another communication channel between languages and the tools that process them.

Altogether, Racket provides a spectrum of tools for creating and experimenting with programming language design. In the
rest of this paper we show three further uses of Racket to design extensions to the base MiniJava language providing solutions
to the 2016 Language Workbench Challenge benchmark problems.

@(figure*
  "mj-while-macro"
  "Definition of while in parenthesized MiniJava"
  @mj-while-macro)

@(figure*
  "mj-parity-compiled"
  "The Parity class compiled to Racket"
  @mj-parity-compiled)

@(figure*
  "mj-new"
  "The implementation of new in parenthesized MiniJava"
  @mj-new)

@(figure*
  "add-tool-tips"
  "Adding tool tips to MiniJava programs"
  add-tool-tips)

@(figure*
  "tool-tips"
  "MiniJava type tool-tips in DrRacket"
  (scale (bitmap "type-tool-tips.png") .75))

@include-section{notation.scrbl}
@include-section{evolution.scrbl}
@include-section{editing.scrbl}
@section[#:tag "conclusion"]{Conclusion}

@(generate-bibliography)