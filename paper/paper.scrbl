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
Racket macro system as if it were a domain-specific language
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
The macro expander finds the outermost expression that is not in core form and applies one step of macro expansion
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

The @emph{#lang} line, as seen at the top of @figure-ref{mj-syntax}, specifies the
module that defines the language in which the program is written, in this case
MiniJava.
Based on this @emph{#lang} line, Racket hands the text of the program over to
the MiniJava reader, which produces a syntax object representing an abstract
syntax tree.
As @figure-ref{mj-impl} shows, the MiniJava reader is composed of a lexer and
parser, as one may find in a standard compiler.
@(figure*
  "typecheck-mod-beg"
  "Typechecking the abstract syntax tree"
  @typecheck-mod-beg)

In general, Racket hands the reader's result to a language's
@racket[#%module-begin] macro, which can perform module-level processing.
This whole-module view gives a language complete control over the expansion of
a module, and enables non-local analysis and transformations---such as type
checking---which are beyond the reach of traditional macros.

Our MiniJava @racket[#%module-begin] form (shown in @figure-ref{typecheck-mod-beg})
simply calls a function, @racket[typecheck-program], which typechecks the AST
and tranlates it to an untyped, prefix, parenthesized version of MiniJava.
@; Constructs in this parenthesized MiniJava are implemented as Racket macros.
@; , which ultimately compile down to pass explicit method tables implemented with
@; Racket vectors.


@; The @racket[#%module-begin] macro
It then splices the resulting syntax into the
body of @emph{#lang racket}'s @racket[#%module-begin].
This allows Racket's expander to take over the rest of the compilation pipeline
and expand the macros mentioned in the previous paragraph into ordinary Racket
code, with Racket conditionals, Racket variables, etc.

@(figure*
  "parenthesized-mj-example"
  "The parenthesized version of the MiniJava example"
  parenthesized-mj-example)
@subsection{Language Constructs as Macros}
@; Once the type checker produces a parenthesized MiniJava program, the next stage of the pipeline is the translation to
@; Racket through macro expansion.

@; Programs in the parenthesized MiniJava language---such as the translation of
@; @figure-ref{mj-syntax} shown in @figure-ref{parenthesized-mj-example}---use
@; the same basic operators as those written in MiniJava's concrete syntax, but
@; use a fully prefixed syntax.
@; Furthermore, type information has been stripped away from definitions, but has
@; been propagated to @racket[send] forms.

@; Despite a superficial resemblence to Racket, parenthesized MiniJava has MiniJava semantics.
Constructs in parenthesized MiniJava are implemented as Racket macros.
Consider the @racket[while] construct used in @figure-ref["parenthesized-mj-example"].
Racket does not have a @racket[while] form that MiniJava could reuse;
@racket[while] must therefore be implemented as part of MiniJava.

@; The @racket[while] expression
@; in @figure-ref{parenthesized-mj-example} cannot be directly implemented as a function since its body may never run and
@; a function would evaluate all of its arguments. To support this translation of @racket[while] loops, we must define
@; @racket[while] as a macro.

As @figure-ref{mj-while-macro} shows, @racket[while] is implemented as a macro which
uses @racket[syntax-parse], a powerful pattern matcher for defining syntactic extensions.
This macro contains a single pattern, @racket[(while test body ...)], which
indicates that it expects to see the identifier @racket[while] followed by a
test expression and any number of body expressions.
The pattern matcher binds @racket[test] and @racket[body] for use within syntax
templates, e.g. expressions contained within @racket[#'⋯] (or @racket[(@#,racket[syntax] ⋯)]).
The @racket[syntax] form is similar to the @racket[quote] form that Racket
inherits from Lisp, with the key differences that the former produces a syntax object
instead of a datum, and supports interpolation.
This interpolation features is used in @racket[while]'s implementation to
support copying pattern variables into a syntax template.

The @racket[while] macro is an example of the kind of linguistic reuse found in
the implementation of many Racket macros:
it expands to Racket's built-in conditional expressions, @racket[when], and Racket's
@racket[letrec] form.
Furthermore, it relies hygiene to prevent the @racket[letrec]-bound variable, @racket[loop], from conflicting with uses
of identically-named variables in the source syntax of @racket[while] forms. Racket's hygienic macro system eases
the job of macro writers, allowing them to write macros without worrying that their bindings will conflict with
those that appear at macro use sites.

@;; while in Java to while in Racket ..
@;; discuss the implementation of the parenthesized while form (without the syntax parameter mess)
@;; this leaves an example to return to later on
@;; reuse racket variable binding or conditional for the MiniJava forms (linguistic reuse)
@subsection{Inter-Macro Communication}
Isolated macro definitions are not sufficient to transform every MiniJava
expression into Racket. The @racket[new] form, for example, requires information
about the class being isntantiated in order to construct an instance with the correct number of fields and a reference to the class's
method table.
@; In this subsection, we present a pattern of communication between macro
@; definitions that allows the definition of macros like @racket[new].
This requires @emph{communication} between the macros that implement class
definition and class instantiation.

To make this concrete, consider the expansion of the @racket[Parity] class from
our running example, which is shown in @figure-ref{mj-parity-compiled}.
At this point, MiniJava forms have been compiled away to ordinary Racket code,
which uses vectors to represent objects and method tables.
The @racket[define-class] form for the @racket[Parity] class expanded to three
definitions: the run-time method table shared by all @racket[Parity] instances,
the constructor that creates instances, and a syntax definition of static
information about the @racket[Parity] class.

So far, we have seen Racket's @racket[define-syntax] form used for macro definitions.
In general, however, @racket[define-syntax] creates @emph{transformer}
bindings, which include not only macros, but also arbitrary bindings whose
values are available at expansion time.

This allows @racket[define-class] to store static information that can be
accessed at compile time by other macros using the @racket[syntax-local-value] procedure.
The implementation of @racket[new] (shown in @figure-ref{mj-new}) can then use
this channel of communication to get access to the name of the constructor,
bound by @racket[define-class], and use it in its own expansion.

@; @Figure-ref{mj-new} gives an example of a macro which consults static information in order to expand. In this case
@; the @racket[new] macro accesses the static information bound to the class identifier passed in and looks up the constructor
@; field of the @racket[static-class-info] record. In the case of the expression @racket[(new Parity)], the use of the
@; @racket[new] macro contains a reference to the identifier @racket[#'Parity]. Using @racket[syntax-local-value], the
@; @racket[new] macro accesses the static information in the @racket[static-class-info] record bound using @racket[define-syntax]
@; to the @racket[Parity] identifier. The @racket[new] macro uses this information to expand into a call to the @racket[Parity:constructor]
@; function to create a new instance of the class.

The @racket[send] macro, used for method calls, also makes use of this technique.
Our MiniJava type checker annotates @racket[send] forms with the name of
the class which it attributes to the receiver.
The @racket[send] macro uses the static information bound to that class name to
determine the correct index into the class's method table.
The bodies of the methods stored in @racket[Parity:method-table], in
@figure-ref{mj-parity-compiled}, show the results of this expansion.

This technique highlights the distinction between Racket's run-time and compile-time phases.
The @racket[new] macro must call @racket[syntax-local-value] at compile-time to
access static class information.
In general, this means that arbitrary, possibly even side-effecting, code may
need to run at compile time to expand a syntactic form. Racket addresses
the issue of mingling compile time code with run-time code through a phase distinction that makes explicit
the execution time of a piece of code. This is necessary to support a tower of linguistic abstractions
that can build upon one another to create more and more sophisticated new languages.

@;; class definitions compiling to define-syntax stuff ..
@;; using define-syntax to store the compile time method table info, etc ...
@;; maybe use the `new` macro as the example here since it is simpler than `send`
@subsection[#:tag "drracket"]{DrRacket Integration}
@;; tooling/tool-tips with type info ... using syntax properties (screen shot)
@;; more than just communication (with compiler and) between our macros, also communicate with tools
@;; Talk about syntax properties a little bit with the example of
@;; type info attached as tool-tips, this sets up well for the refactoring later

Transformer bindings are but one of the communication mechanisms available to
Racket macros.
As discussed above, Racket's syntax objects are rich data structures that
store more than just symbolic representations of program fragments;
macro programmers can attach arbitrary key-value pairs to syntax objects using
@emph{syntax properties}.
Just as transformer bindings allow communication between macros in different
@emph{locations}, syntax properties enable communication between different
processing @emph{passes}---including external tools---at the same location.

Our implementation of MiniJava uses syntax properties to attach type information to expressions, which 
the DrRacket programming environment can present as mouse-over tool-tips, as @figure-ref{tool-tips} shows.
DrRacket knows to look for @racket['mouse-over-tooltips] syntax properties in
the expansion of programs, and uses their contents for its display.

Similarly, DrRacket's @emph{check syntax} tool, which draws the binding arrows in @figure-ref{tool-tips},
uses syntax properties to variable bindings and uses that are present in a
source program, but absent in its expansion.
As a concrete example, because MiniJava class fields are compiled away into
vector offsets (as opposed to Racket variables), a
@racket['disappeared-binding] syntax property records the presence of field
bindings in the source program.

@; @subsection{The Racket Language Workbench}
@; The previous subsections highlight many features of Racket that make it suitable for use as a language workbench.
@; An extensible language in which programmers can freely add new language forms gives programmers the power to express complex
@; problems in a domain specific fashion. The ability to define macros that can query compile-time state in addition to
@; their arguments allows for complex extensions to the language and one powerful communication channel for implementing
@; cooperating macros. Access to the full Racket programming language at both compile-time and run-time allows for expressive
@; extensions to the compiler that do not limit the programmer. Being able to run arbitrary code at compile time allows a
@; number of approaches to language implementation. Finally, syntax objects and their attached syntax
@; properties allow for yet another communication channel between languages and the tools that process them.

@; Altogether, Racket provides a spectrum of tools for creating and experimenting with programming language design. In the
@; rest of this paper we show three further uses of Racket to design extensions to the base MiniJava language providing solutions
@; to the 2016 Language Workbench Challenge benchmark problems.

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