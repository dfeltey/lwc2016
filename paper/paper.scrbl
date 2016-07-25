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
@;;  - Visual Studio Code, Lnaguage server protocol


@title{Language Workbench Challenge 2016: Racket Submission}

@abstract{
We present a submission to the 2016 Language Workbench Challenge using
the Racket programming language. Racket provides a wealth of features
that aid in the process of language design including a mechanism for
dispatching on a language implementation and a powerful linguistic
extension system. To address the 2016 language workbench challenge
benchmark problems we have implemented a version of the MiniJava
programming language relying on these core features of Racket.
Building upon the implementation of MiniJava we demonstrate the
use of Racket as a language workbench for building and extending
programming languages.
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

In this section, we discuss the use of Racket as a language workbench using our implementation
of MiniJava as a guiding example. Additionally, this section provides the necessary background for presenting
our solutions to the benchmark problems. We begin with a high-level overview of Racket then discuss several
of its features in depth to illustrate one approach to language development in Racket.

@subsection[#:tag "racket-overview"]{An Overview of Racket and its Macro System}
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
the @emph{read} pass, turns a textual program into a tree of syntax objects. Syntax objects are rich data structures that
combine a symbolic representation of a program with a number of properties. Properties on syntax objects can
include source location information and lexical binding information, as well as arbitrary information that
programmers may attach and access using Racket's @racket[syntax-property] procedure.

After a program has been read, the @emph{expansion} process invokes macros to translate the syntax objects into a
low-level programming language that looks like a lambda calculus with conditionals, local binding forms, and
many primitive functions. In contrast to function evaluation, in which arguments must
be evaluated before a function is applied, in an inside-out process, the expansion process happens outside-in.
The macro expander finds the outermost expression that is not in the core form and applies one step of macro expansion
to it, step by step, eliminating the use of macros from the program.

Racket's facilities for language building and its rich core library allow programmers to implement entirely new
languages on top of Racket. A notable example of this is Typed Racket, a gradually-typed sister language of Racket.
Languages built on top of Racket, however, do not need to share Racket's syntax or even its semantics as demonstrated
by implementations of Algol 60, Datalog, and Scribble, a language for formatting prose, on top of Racket.

The implementation of new languages is made possible by Racket's expressive syntax system along with Racket's module
system. In Racket a new language is merely a module that declares a @emph{reader} along with bindings for the language's
syntax. The module system additionally provides namespace control allowing programmers to rename and remove bindings, thus
allowing languag definitions that are not just supersets of Racket.
In the rest of this section we demonstrate several of Racket's features for building languages and their use in the
implementation of MiniJava.


@subsection{#lang Languages}
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
an untyped, prefix, parenthesized version of MiniJava with each MiniJava construct corresponding to a macro.
In place of a traditional code generation phase, the implementation relies on Racket's macro system to transform
the parenthesized MiniJava classes into method tables implemented with Racket's vector data type. Wherever possible
MiniJava forms are translated directly into the corresponding Racket form to enable linguistic reuse, particular examples
of this include Racket's @racket[if] and binding forms.

@Figure-ref{mj-syntax} shows an example MiniJava program.
The @emph{#lang} form, at the top, controls dispatching to the correct language implementation,
specifically the language's reader and its syntax definitions.
The syntax consists of the @emph{#lang} form followed by the name of the module that defines a language.
This mechanism allows the language to specify its own reader which may differ from Racket's.
In the case of MiniJava the @emph{mini-java} language specifies a standard Java-style lexer and parser
which are called on the concrete syntax to produce an abstract syntax tree. As described in the previous
subsection, this abstract syntax tree is the result of the @emph{read} pass.

@(figure*
  "typecheck-mod-beg"
  "Typechecking the abstract syntax tree"
  @typecheck-mod-beg)

Once a MiniJava program has been read to produce an abstract syntax tree, the type checking phase is
implemented as a whole-module transformation. @Figure-ref{typecheck-mod-beg} shows the definition of @racket[module-begin]
which performs type checking and translates the abstract syntax tree into parenthesized MiniJava. This module exports
the definiton of @racket[module-begin] as @racket[#%module-begin], the @racket[#%module-begin] form is one of Racket's
core forms which is implicitly added around the body of a program during expansion. Replacing Racket's definition of
@racket[#%module-begin] allows for complete control over the expansion of the module, this allows a whole-module view of
a program which cannot be accomplished with traditional macros. In our type checker, the
@racket[module-begin] form simply calls a function, @racket[typecheck-program], which traverses the abstract syntax
performing type checking and producing the parenthesized form of the MiniJava program. The resulting syntax from the
type checking function is then spliced into the body of @emph{#lang racket}'s @racket[#%module-begin], allowing Racket's expander
to take over the rest of the compilation pipeline.

@(figure*
  "parenthesized-mj-example"
  "The parenthesized version of the MiniJava example"
  parenthesized-mj-example)
@subsection{Language Constructs as Macros}
Once the type checker produces a parenthesized MiniJava program, the next stage of the pipeline is the translation to
Racket through macro expansion. This section describes the implementation of MiniJava language features as Racket macros.

The result of lexing, parsing, and type checking the program in @figure-ref{mj-syntax} produces the parenthesized
MiniJava program in @figure-ref{parenthesized-mj-example}. Programs in the parenthesized MiniJava language have a
similar structure to those written in MiniJava's concrete syntax. The two languages share the same basic operators
with the key differences being that the syntax of parenthesized MiniJava is fully prefixed and type information has
been stripped away except for method calls using the @racket[send] form which have been annotated with type information.

Although the parenthesized MiniJava language appears more similar to Racket than MiniJava does, its semantics is still
that of MiniJava. Consider the @racket[while] construct used in @figure-ref["mj-syntax" "parenthesized-mj-example"].
Because Racket does not have a @racket[while] form, it must be implemented as part of MiniJava. The @racket[while] expression
in @figure-ref{parenthesized-mj-example} cannot be directly implemented as a function since its body may never run and
a function would evaluate all of its arguments. To support this translation of @racket[while] loops, we must define
@racket[while] as a macro.

The implementation of the @racket[while] form is shown in @figure-ref{mj-while-macro}. The definition of @racket[while]
uses @racket[syntax-parse] which provides a powerful mechanism for defining new syntactic extensions via pattern matching
and templating.The definition of @racket[while] contains a single
pattern to match, @racket[(while test body ...)], which indicates that the @racket[while] macro expects to see the identifier
@racket[while] followed by a test expression and any number of body expressions. The pattern matching mechanism binds
@racket[test] and @racket[body] for use within syntax templates, e.g. the expression contained within @racket[#'(⋯)].
The expression @racket[#'expr] is equivalent to @racket[(@#,racket[syntax] expr)] and is similar to the @racket[quote] form that Racket
inherits from Lisp. The key difference between @racket[quote] and @racket[syntax] is that the latter produces a syntax object
rather than a datum and supports interpolation as seen in the @racket[while] definition to support copying pattern variables
into a syntax template.

The @racket[while] macro is an example of the kind of linguistic reuse found in the implementation of many
Racket macros. The @racket[while] form relies on Racket's built in conditional expressions, @racket[when], and Racket's
@racket[letrec] form in order to implement MiniJava's @racket[while] statement. Furthermore, this example makes use of
Racket's hygienic macro system preventing the @racket[letrec]-bound variable, @racket[loop], from conflicting with uses
of a similarly named variable that appears in the syntax of the @racket[while] form. Racket's hygienic macro system eases
the job of macro writers, allowing them to write new macros without worrying that their use of binding forms will conflict with
variables that appear in the use of a macro.

@;; while in Java to while in Racket ..
@;; discuss the implementation of the parenthesized while form (without the syntax parameter mess)
@;; this leaves an example to return to later on
@;; reuse racket variable binding or conditional for the MiniJava forms (linguistic reuse)
@subsection{Inter-Macro Communication}
Macros provide a powerful way of specifying the translation from parenthesized MiniJava forms into Racket, but isolated
macro definitions are not expressive enough to transform every MiniJava expression into Racket. The creation of object instances
using @racket[new],for example, cannot be implemented as a stand-alone macro definition. The @racket[new] form requires information
about the class being isntantiated in order to construct an instance with the correct number of fields and a reference to the class's
method table. In this subsection, we present a pattern of communication between macro definitions that allows the definition of macros
like @racket[new].

After a parenthesized MiniJava program is produced, Racket's expander takes over and expands class definitions.
For example, the @racket[Parity] class defined in @figure-ref{parenthesized-mj-example} compiles into the Racket
code in @figure-ref{mj-parity-compiled}. When the @racket[define-class] form expands it produces
three definitions. The first definition is the run-time method table which  is shared by all instances of
the @racket[Parity] class. The second definition is the constructor that creates instances of the @racket[Parity]
class. Finally, the third definition is bound using Racket's @racket[define-syntax] form to static information
about the @racket[Parity] class.

In the previous subsection, we used Racket's @racket[define-syntax] form to define a new macro that implements
MiniJava's @racket[while] statement. In general, however, the @racket[define-syntax] form creates a @emph{transformer}
binding. Transformer bindings include macros, as in the definition of @racket[while], but more specifically the
@racket[define-syntax] form creates bindings whose values are available at expansion time. This allows the use of the
@racket[define-syntax] to store static information that can be accessed at compile time using the @racket[syntax-local-value]
procedure. This allows for macros to expand into definitions using @racket[define-syntax] which can be used as a communication
channel between distinct macros that can be consulted to determine their expansion.

@Figure-ref{mj-new} gives an example of a macro which consults static information in order to expand. In this case
the @racket[new] macro accesses the static information bound to the class identifier passed in and looks up the constructor
field of the @racket[static-class-info] record. In the case of the expression @racket[(new Parity)], the use of the
@racket[new] macro contains a reference to the identifier @racket[#'Parity]. Using @racket[syntax-local-value], the
@racket[new] macro accesses the static information in the @racket[static-class-info] record bound using @racket[define-syntax]
to the @racket[Parity] identifier. The @racket[new] macro uses this information to expand into a call to the @racket[Parity:constructor]
function to create a new instance of the class.

Another use of this static information occurs in the @racket[send] macro. The typed elaboration of a MiniJava program
produced by our lexer, parser, and type checker annotates @racket[send] method calls with the name of the class to which 
the method belongs. In this case the @racket[send] macro uses the static information to resolve method calls into
the correct indices in the class's method table. @Figure-ref{mj-parity-compiled} shows the result of this
expansion within the functions stored in @racket[Parity:method-table].

This communication pattern between macros also exemplifies the distinction between Racket's run-time and compile-time phases.
The @racket[new] macro must call the @racket[syntax-local-value] function at compile-time in order to query the static information
needed. In general, this means that arbitrary code, including side-effecting code, may need to run at compile time in order to
expand a syntactic form. Racket addresses
the issue of mingling compile time code with run-time code through a phase distinction that makes explicit
the execution time of a piece of code. This is necessary in order to support a tower of linguistic abstractions
that can easily build upon one another to create new languages.

@;; class definitions compiling to define-syntax stuff ..
@;; using define-syntax to store the compile time method table info, etc ...
@;; maybe use the `new` macro as the example here since it is simpler than `send`
@subsection[#:tag "drracket"]{DrRacket Integration}
@;; tooling/tool-tips with type info ... using syntax properties (screen shot)
@;; more than just communication (with compiler and) between our macros, also communicate with tools
@;; Talk about syntax properties a little bit with the example of
@;; type info attached as tool-tips, this sets up well for the refactoring later
The use of Racket's @racket[define-syntax] form allows a communication channel between macros, but Racket's
syntax system, more generally, allows communication between languages and external tools through the
use of @emph{syntax properties}. As discussed above, Racket's syntax objects are a rich data structure that
store more than just a symbolic representation of a program fragment. Furthermore programmers can attach arbitrary
data to syntax objects using the @racket[syntax-property] function.

Our implementation of MiniJava uses syntax properties to attach type information to expressions that 
the DrRacket programming environment can present as mouse-over tool-tips. @Figure-ref{tool-tips} shows a MiniJava program
in DrRacket displaying a type tool-tip for the @racket[check] identifier.
The mouse-over tool tips are integrated in DrRacket using the syntax property mechanism, DrRacket traverses the fully-expanded
program and determines where tool-tips belong based on the @racket['mouse-over-tooltips] syntax property.

Whereas @racket[define-syntax] allows for communication and cooperation between macros, syntax properties also allow language
implementors to collaborate with external tools and libraries that may process or otherwise analyze programs the compile
into Racket. Additionally, DrRacket's @emph{check syntax} tool, which draws the arrows between identifiers in @figure-ref{tool-tips},
uses syntax properties to track uses and binding positions of variables in a source program which may not appear in the fully-expanded
syntax object. As a concrete example, because fields in a MiniJava class are compiled away into vector offsets, a syntax property
with the key @racket['disappeared-binding] is used to record that a binding is present in the source syntax but has been
compiled away in the fully-expanded program.

@subsection{The Racket Language Workbench}
The previous subsections highlight many features of Racket that make it suitable for use as a language workbench.
An extensible language in which programmers can freely add new language forms gives programmers the power to express complex
problems in a domain specific fashion. The ability to define macros that can query compile-time state in addition to
their arguments allows for complex extensions to the language and one powerful communication channel for implementing
cooperating macros. Access to the full Racket programming language at both compile-time and run-time allows for expressive
extensions to the compiler that do not limit the programmer. Being able to run arbitrary code at compile time allows a
number of approaches to language implementation. Finally, syntax objects and their attached syntax
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
@;; cite Fowler's website
Fowler's definition of a language workbench requires:
@itemlist[@item{Free definition of new languages which fully integrate with one another}
          @item{An abstract representation as the primary source of information}
          @item{DSLs defined as a schema, an editor, and generators}
          @item{DSLs can be manipulated with a projectional editor}
          @item{Persisting incomplete or contradictory information in the abstract representation}]
Racket's communicating macros and @emph{#lang} mechanism enable the free definition of new languages.
Syntax objects comprise an abstract repsentation of programs implemented on top of Racket and can store user specified
information in syntax properties. Racket's @emph{#lang} feature enables the composition of language readers and
bindings which implement the language's semantics, whereas DrRacket's plugin mechanism provides an extensible
editor for manipulating programs that compile down to Racket. Although, DrRacket does not provide a projectional
editing mechanism, its plugin mechanism allows extensions to support program editing. Finally, the syntax object
representation may persist incomplete or contradictory information due to macro expansion introducing syntax from
private modules or rearranging pieces of syntax.

In the previous sections we have introduced the features of Racket that make it suitable for use as a language workbench.
We have shown how Racket's @emph{#lang} mechanism is used to connect the components of a language implementation and
how Racket's macro system allows programmers to freely extend a programming language to suit their needs. The implementation
of MiniJava demonstrates how these features work together to build new languages upon Racket.

Through the solution of threee benchmark problems we further explore how languages built in Racket can be extended with
tabular notation, restricted syntax extensions, and tool support. Racket' ability to specify a language's reader through
the @emph{#lang} mechanism allows a new tabular notation for state machines in MiniJava. Syntax parameters allow the addition
of new language forms that are only valid within certain contexts. Finally, syntax properties and DrRacket's plugin mechanism
enable the creation of new language-specific tools. The Racket ecosystem readily allows the composition of these features
to build and extend programming languages.

@(generate-bibliography)