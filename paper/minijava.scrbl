#lang scribble/sigplan @10pt

@(require pict/code
          (only-in pict bitmap scale)
          "setup.rkt"
          "bib.rkt"
          "mj-examples.rkt"
          scriblib/figure
          (only-in scribble/core style) scribble/latex-properties
          (only-in scribble/manual racket racketblock hash-lang)
          (only-in racket/format ~a))

@title[#:tag "sec:minijava"]{MiniJava via Racket}



In this section, we discuss the use of Racket as a language workbench using our implementation
of MiniJava as a guiding example. Additionally, this section provides the necessary background for presenting
our solutions to the benchmark problems. We begin with a high-level overview of Racket then discuss several
of its features in depth to illustrate one approach to language development in Racket.

@section[#:tag "racket-overview"]{An Overview of Racket and its Macro System}
@;; brief overview of expansion and syntax objects to set up discussion of syntax properties
@;; later on, and the built in forms that we can rely on to implement languages
@;; (such as syntax-parameters which will be discussed later in the evolution section)
Racket is a dynamically typed@~cite[dynamic-typing], call-by-value programming language in
the Lisp family. One of Racket's most prominent features is
its powerful hygienic macro system@~cite[hygienic-macros] and it is central to the
consideration of Racket as a language workbench. While the
macro system is clearly an evolution of the early
Assembly@~cite[mcilroy] and Lisp@~cite[lisp-macros] macro systems,
it has changed so much that it is nearly unrecognizable as a
descendant of them@~cite[scopes]. Instead, it is better to approach the
Racket macro system as a domain-specific language
for writing an extensible compiler front-end. Racket macro
programmers start from the compiler for some initial
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

Whereas a typical compiler's front end parses a textual program into
an abstract syntax tree of meaningful nodes, Racket separates the
process into two passes: the @emph{reader} and the
@emph{expander}. The reader turns a textual program into a syntax tree
of uninterpreted nodes. The expander rewrites instances of derived
syntactic forms until none remain; the resulting @emph{fully-expanded}
syntax tree uses only the syntax of core Racket, a programming
language that looks like a lambda calculus with conditionals, local
binding forms, and many primitive functions. The expander does not
know the binding structure or even the expression structure of derived
syntactic forms @emph{a priori}; rather, it discovers it as it
works. To contrast expansion with function evaluation, where the
arguments to the function are evaluated before the function is
applied, the expansion process happens outside-in: the expander finds
the outermost expression that is a derived form, rewrites it using the
associated transformation rule, and continues.

The tree-based representation that the reader produces and the expander processes is called a @emph{syntax object}.
Syntax objects@~cite[syntactic-abstraction-in-scheme] are rich data structures that
combine a symbolic representation of a program with a number of properties. Properties on syntax objects
include original source locations, lexical binding information, as well as arbitrary data that
programmers may attach and access using Racket's @racket[syntax-property] procedure.

The implementation of new languages is made possible by this expressive syntax system along with Racket's module
system@~cite[you-want-it-when]. In Racket, a language is merely a module that declares a reader along with bindings for the language's
syntax. Modules provide namespace control, allowing programmers to rename and remove bindings and thus create
languages that are not just supersets of Racket.

@(figure
  "mj-impl"
  "Structure of the MiniJava implemntation"
  @pipeline-diagram)
@(figure
  "mj-syntax"
  "A sample MiniJava program"
  @mj-simple-example)
@(figure*
  "typecheck-mod-beg"
  "Typechecking the abstract syntax tree"
  @typecheck-mod-beg)

These facilities, along with a rich core library, allow programmers to implement entirely new
languages on top of Racket. A notable example of this is Typed Racket@~cite[cthf-sfp-2007 thf-popl-2008 tr-diss], a gradually-typed sister language of Racket.
Languages built on top of Racket, however, do not need to share Racket's concrete syntax or even its semantics, as demonstrated
by implementations of Algol 60, Datalog, and Scribble@~cite[scribble], a language whose concrete syntax is designed for
writing prose (and is used to implement this paper).
 
In the rest of this section we demonstrate several of Racket's features for building languages and their use in our
implementation of MiniJava. Following the guide of @figure-ref{mj-impl}, in @secref["lang"] we describe the
lexing, parsing, and type checking process that transforms the MiniJava program in @figure-ref{mj-syntax} into the program
in the left half of @figure-ref{expansion}. In sections 2.3 and 2.4 we explain
the pieces of Racket's macro system that cooperate to translate the program in the left-hand side of @figure-ref{expansion}
into the program in the right-hand side. Finally, @secref["drracket"] illustrates DrRacket's use of fully-expanded
programs to convey meta-information to programmers.

@section[#:tag "lang"]{@tt{#lang} Languages}
@;; give the high level overview of the diagram and how each piece fits together ...???
@;; compare tradition compiler pipeline with #lang mechanism ...
@;; describe a bit about the #lang mechanism how it allows implementing totally new languages
@;; uses #lang to find the lexer/parser/typechecking ...
@;; compare with TR/scribble/algol60 etc, languages implemented in Racket with totally different
@;; semantics
@;; use the module-begin from the typechecker as an example in the prose
@;; module system, selective exports language is a set of bindings some of which are recognized specifically
Racket programs must begin with the token @tt{#lang} followed by the name of a module that defines a language.
Racket forwards the program's contents to the language module for language-specific processing, a mechanism we refer to as @emph{linguistic dispatch}.
@;;Through Racket's linguistic dispatch, the language module receives the contents of the program for language-specific processing.
The @tt{#lang} line at the top of @figure-ref{mj-syntax}, specifies that the program is written in MiniJava.
As such, Racket hands the text of the program over to
the MiniJava reader, which produces a syntax object representing an abstract syntax tree.
As @figure-ref{mj-impl} shows, the MiniJava reader is composed of a lexer and
parser, as one may find in a standard compiler.


In general, Racket hands the reader's result to a language's
@racket[#%module-begin] macro, which can perform module-level processing.
This whole-module view gives a language complete control over the expansion of
a module, and enables non-local analysis and transformations---such as type
checking@~cite[langs-as-libs]---which are beyond the reach of traditional macros.

Our MiniJava @racket[#%module-begin] form (shown in @figure-ref{typecheck-mod-beg})
simply calls a function, @racket[typecheck-program], which typechecks the AST
and translates it to an untyped, prefix parenthesized version of MiniJava.
@; Constructs in this parenthesized MiniJava are implemented as Racket macros.
@; , which ultimately compile down to pass explicit method tables implemented with
@; Racket vectors.


@; The @racket[#%module-begin] macro
It then splices the resulting syntax into the
body of @tt{#lang racket}'s @racket[#%module-begin].
This allows Racket's expander to take over the rest of the compilation pipeline
and expand the program into ordinary Racket
code that uses Racket conditionals, Racket variables, etc.

@(figure*
  "expansion"
  "The Runner class in parenthesized MiniJava and its Racket expansion"
  expansion-figure)
@;{@(figure*
  "parenthesized-mj-example"
  "The parenthesized version of the MiniJava example"
  parenthesized-mj-example)}
@section[#:tag "macros"]{Language Constructs as Macros}
@; Once the type checker produces a parenthesized MiniJava program, the next stage of the pipeline is the translation to
@; Racket through macro expansion.

@; Programs in the parenthesized MiniJava language---such as the translation of
@; @figure-ref{mj-syntax} shown in @figure-ref{parenthesized-mj-example}---use
@; the same basic operators as those written in MiniJava's concrete syntax, but
@; use a fully prefixed syntax.
@; Furthermore, type information has been stripped away from definitions, but has
@; been propagated to @racket[send] forms.

@; Despite a superficial resemblence to Racket, parenthesized MiniJava has MiniJava semantics.
To get a sense of how macro-based expansion works, lets start with a straightforward one:
the @racket[while] macro. A use of it appears on line 7 in the left half of @figure-ref{expansion}.

@; The @racket[while] expression
@; in @figure-ref{parenthesized-mj-example} cannot be directly implemented as a function since its body may never run and
@; a function would evaluate all of its arguments. To support this translation of @racket[while] loops, we must define
@; @racket[while] as a macro.
@(figure
  "mj-while-macro"
  "Definition of while in parenthesized MiniJava"
  @mj-while-macro)
@(figure
  "mj-new"
  "The implementation of new in parenthesized MiniJava"
  @mj-new)

The implementation of MiniJava's @racket[while], in @figure-ref{mj-while-macro}, uses Racket's @racket[define-syntax] form to bind @racket[while]
to a @emph{transformer function} that implements the compilation step for @racket[while] expressions.
Transformer functions consume and produce syntax; they are invoked by the expander during the
macro expansion process.
It is important to note that while @racket[define-syntax] is a special kind of binding form, it is
not only used to define macros. The expander specially recognizes that @racket[while] is bound
to a function and that is what triggers the macro expansion of @racket[while] forms, but
@racket[define-syntax] can bind variable to other values too (we exploit this power in
the next section to handle @racket[define-class]).
The @racket[while] transformer function's implementation uses @racket[syntax-parse],
a powerful pattern matcher for defining syntactic extensions@~cite[fortifying-macros].
This macro contains a single pattern, @racket[(while test body ...)], which
indicates that it expects to see the identifier @racket[while] followed by a
test expression and any number of body expressions.
The pattern matcher binds @racket[test] and @racket[body] for use within syntax
templates, e.g. expressions contained within @racket[#'⋯] (or @racket[(@#,racket[syntax] ⋯)]).
The @racket[syntax] form is similar to the @racket[quote] form that Racket
inherits from Lisp, with the key difference that the former produces a syntax object
instead of a datum, and supports interpolation.
This interpolation feature is used in @racket[while]'s implementation to
support copying pattern variables into a syntax template.

@Figure-ref{expansion} shows the parenthesized version of the @racket[Runner] class from
@figure-ref{mj-syntax} and it's expansion into Racket.
The @racket[while] form on the left of @figure-ref{expansion} expands into the @racket[letrec] expression,
spanning lines 7 through 25 on the right.
As the definition of @racket[while] specifies, the loop's condition becomes the guard to Racket's @racket[when]
and the body is copied into the body of @racket[when] before further expansion occurs.

The @racket[while] macro is an example of the kind of linguistic reuse found in
the implementation of many Racket macros:
it expands to Racket's built-in conditional expressions, @racket[when], and Racket's
@racket[letrec] form.
Furthermore, it relies on hygiene@~cite[hygienic-macros] to prevent the @racket[letrec]-bound variable, @racket[loop], from conflicting with uses
of identically-named variables in the source syntax of @racket[while] forms. Hygiene eases
the job of macro writers, allowing them to write macros without worrying that their bindings will conflict with
those that appear at macro use sites. In addition to supporting local variables with hygiene,
Racket's expansion process also ensures that free variables in the syntax object in the template
(@racket[letrec], @racket[λ], and @racket[when] in this case) refer to the bindings in scope
at the definition of the macro, not at the use of the macro@~cite[macros-that-work].

@;; while in Java to while in Racket ..
@;; discuss the implementation of the parenthesized while form (without the syntax parameter mess)
@;; this leaves an example to return to later on
@;; reuse racket variable binding or conditional for the MiniJava forms (linguistic reuse)
@section[#:tag "comm"]{Inter-Macro Communication}
Isolated macro definitions are not sufficient to transform every MiniJava
expression into Racket. The @racket[new] form, for example, requires information
about the class being instantiated in order to construct an instance with the correct number of fields and a reference to the class's
method table.
@; In this subsection, we present a pattern of communication between macro
@; definitions that allows the definition of macros like @racket[new].
This requires @emph{communication} between the macros that implement class
definition and class instantiation@~cite[mtwt].

To make this concrete, consider the expansion of the @racket[Parity] class from
our running example, which is shown on the right of @figure-ref{expansion} beginning at line 40.
At this point, MiniJava forms have been compiled away to ordinary Racket code,
which uses vectors to represent objects and method tables.
The @racket[define-class] form for the @racket[Parity] class expands to three
definitions: the run-time method table shared by all @racket[Parity] instances,
the constructor that creates instances, and a syntax definition of compile-time
information about the @racket[Parity] class used to guide the expansion of other
forms that refer to @racket[Parity].

The first definition, @racket[Parity:runtime-method-table] (@figure-ref{expansion} line 40), is a vector
storing the methods @racket[is_odd] and @racket[is_even].
The constructor definition, @racket[Parity:constructor] on line 53, is a function of no arguments for
creating new instances of the @racket[Parity] class. Because the @racket[Parity] class has no fields,
its instance vectors contain only a reference to the method table.
The definition of @racket[Parity] on line 56 is the third binding introduced by the expansion of @racket[define-class].
It is bound, using @racket[define-syntax], to a record, @racket[static-class-info], that stores compile-time information about the class.
In general the compile-time definitions for classes contain an identifier bound to the parent class
information (or @racket[#f] if there is no parent class, as in this case),
a table mapping method names to vector offsets, syntax objects referring to the run-time method table and constructor identifiers,
and an integer indicating the number of fields in the class.

In the previous subsection, we use Racket's @racket[define-syntax] form to bind @racket[while] to
a transformer function. In general, however, @racket[define-syntax] can bind identifiers to
arbitrary values, which are then available at expansion time@~cite[mtwt].

This allows @racket[define-class] to store static information that can be
accessed at compile-time by other macros using the @racket[syntax-local-value] procedure.
In general, if a macro has access to the identifier bound by @racket[define-syntax], then it
can pass that identifier to @racket[syntax-local-value] to look up the value.
The implementation of @racket[new] (shown in @figure-ref{mj-new}) uses
this channel of communication to get access to the name of the constructor,
bound by @racket[define-class], and use it in its own expansion. More precisely,
it uses @racket[syntax-local-value] to get the @racket[static-class-info] record
bound to the class. The function @racket[static-class-info-constructor-id] is simply
a field accessor (which would be written as the @tt{.constructor_id} suffix in other languages)
that returns the value of the second-to-last field in the record (the identifier @racket[#'Parity:constructor]
in the case of the @racket[Parity] class). Then, the macro uses @tt{#,} to drop that identifier
into a pair of parentheses that form an application expression.
Line 5 of @figure-ref{expansion} shows the expansion of @racket[(new Parity)] on the left into a call
to @racket[Parity:constructor] on the right.

@; @Figure-ref{mj-new} gives an example of a macro which consults static information in order to expand. In this case
@; the @racket[new] macro accesses the static information bound to the class identifier passed in and looks up the constructor
@; field of the @racket[static-class-info] record. In the case of the expression @racket[(new Parity)], the use of the
@; @racket[new] macro contains a reference to the identifier @racket[#'Parity]. Using @racket[syntax-local-value], the
@; @racket[new] macro accesses the static information in the @racket[static-class-info] record bound using @racket[define-syntax]
@; to the @racket[Parity] identifier. The @racket[new] macro uses this information to expand into a call to the @racket[Parity:constructor]
@; function to create a new instance of the class.

The @racket[send] macro, used for method calls, also makes use of this technique.
Our MiniJava type checker annotates @racket[send] forms with the name of
the class of the receiver (which it computes during type checking).
The @racket[send] macro uses the static information bound to that class name to
determine the correct index into the class's method table.
The bodies of the methods stored in @racket[Parity:runtime-method-table], in
@figure-ref{expansion}, show the results of this expansion.
Line 44 of @figure-ref{expansion} shows a method call to @racket[is_even] on the left
and the @racket[let*] expression it transforms into on the right. The @racket[1] on
line 45 is computed by using @racket[syntax-local-value] to get the @racket[static-class-info]
(as with @racket[new]) and then looking up @racket[is_even] in the table on line 59.

This technique highlights the distinction between Racket's run-time and compile-time phases@~cite[you-want-it-when].
The @racket[new] and @racket[send] macros must call @racket[syntax-local-value] at compile-time to
access static class information.
In general, this means that arbitrary, possibly even side-effecting, code may
need to run at compile-time to expand a syntactic form. Racket addresses
the issue of mingling compile-time code with run-time code through a phase distinction that makes explicit
the execution time of a piece of code. This is necessary to support a tower of linguistic abstractions
that can build upon one another to create more and more sophisticated new languages.

@;; class definitions compiling to define-syntax stuff ..
@;; using define-syntax to store the compile-time method table info, etc ...
@;; maybe use the `new` macro as the example here since it is simpler than `send`
@section[#:tag "drracket"]{DrRacket Integration}
@;; tooling/tool-tips with type info ... using syntax properties (screen shot)
@;; more than just communication (with compiler and) between our macros, also communicate with tools
@;; Talk about syntax properties a little bit with the example of
@;; type info attached as tool-tips, this sets up well for the refactoring later

Transformer bindings are but one of the communication mechanisms available to
Racket macros@~cite[mtwt].
As discussed above, Racket's syntax objects are rich data structures that
store more than just symbolic representations of program fragments;
macro programmers can attach arbitrary key-value pairs to syntax objects using
@emph{syntax properties}.
Just as transformer bindings allow communication between macros in different
@emph{locations}, syntax properties enable communication between different
processing @emph{passes}---including external tools---at the same location@~cite[langs-as-libs].

@(figure
  "tool-tips"
  "MiniJava type tool-tips in DrRacket"
  (scale (bitmap "type-tool-tips.png") .75))

Our implementation of MiniJava uses syntax properties to attach type information to expressions, which 
the DrRacket programming environment@~cite[drracket] can present as mouse-over tool-tips, as @figure-ref{tool-tips} shows.
DrRacket knows to look for @racket['mouse-over-tooltips] syntax properties in
the expansion of programs, and uses their contents for its display.

Similarly, DrRacket's check syntax tool, which draws the binding arrows in @figure-ref{tool-tips},
uses syntax properties to highlight variable bindings and uses that are present in a
source program, but absent in its expansion.
As a concrete example, because MiniJava class fields are compiled away into
vector offsets (as opposed to Racket variables), a
@racket['disappeared-binding] syntax property records the presence of field
bindings in the source program. @Figure-ref{expansion} shows this explicitly,
the definition of the @racket[check] field on line 2 is absent from the expanded program
and the reference to @racket[check] on line 9 compiles into the expression @racket[(vector-ref this 1)]
on line 11 of the expanded program. Despite this, DrRacket uses the @racket['disappeared-binding] property
to associate the definition and uses of @racket[check] in @figure-ref{tool-tips}.


@; @subsection{The Racket Language Workbench}
@; The previous subsections highlight many features of Racket that make it suitable for use as a language workbench.
@; An extensible language in which programmers can freely add new language forms gives programmers the power to express complex
@; problems in a domain specific fashion. The ability to define macros that can query compile-time state in addition to
@; their arguments allows for complex extensions to the language and one powerful communication channel for implementing
@; cooperating macros. Access to the full Racket programming language at both compile-time and run-time allows for expressive
@; extensions to the compiler that do not limit the programmer. Being able to run arbitrary code at compile-time allows a
@; number of approaches to language implementation. Finally, syntax objects and their attached syntax
@; properties allow for yet another communication channel between languages and the tools that process them.

@; Altogether, Racket provides a spectrum of tools for creating and experimenting with programming language design. In the
@; rest of this paper we show three further uses of Racket to design extensions to the base MiniJava language providing solutions
@; to the 2016 Language Workbench Challenge benchmark problems.


@;{@(figure*
  "mj-parity-compiled"
  "The Parity class compiled to Racket"
  @mj-parity-compiled)}




@;{@(figure*
  "add-tool-tips"
  "Adding tool tips to MiniJava programs"
  add-tool-tips)}

