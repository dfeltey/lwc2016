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

The Racket language proper consists of a module-oriented, untyped
functional language in the spirit of Lisp and Scheme and the syntax system
introduced in the preceding section. In contrast, MiniJava
(@secref{sub:minijava}) is a simplistic class-based object-oriented
programming language in the spirit of Java. Using Racket's syntax system,
it is nevertheless possible to create a realistic implementation of
MiniJava with a relatively small effort---thanks to linguistic reuse.

@figure-here["fig:mj-syntax" "A sample MiniJava program"]{
 @mj-simple-example}

This section provides an overview of the MiniJava implementation
(@secref{sub:overview}) followed by a presentation of the individual
building blocks. Where necessary, the section also explains Racket features
on a technical basis.

@; -----------------------------------------------------------------------------
@section[#:tag "sub:minijava"]{MiniJava}

@Figure-ref{fig:mj-syntax} displays a MiniJava program. Like any Java
program, it consists of a series of class definitions, one of them
designated as the ``main'' class. Classes have public and private methods
and fields. Each of the latter comes with type signatures, where types are
the names of classes. The body of a method may use the familiar statements
of an imperative language: assignments, @tt{if} conditionals, and
@tt{while} loops. MiniJava expressions are also the familiar ones from
Java.  

MiniJava lacks many of Java's sophisticated features: inheritance, abstract
classes, interfaces, packages, etc. The goal is to help students in
introductory courses, not to model the complexities of a real-world
language.

@; -----------------------------------------------------------------------------
@section[#:tag "sub:overview"]{From MiniJava to Core Racket: an Overview}

Whereas a typical compiler's front end parses a textual program into an
abstract syntax tree of meaningful nodes, an incremental Racket
implementation separates this process into two distinct steps: the reader
and the expander. The @emph{reader} turns a textual program into a syntax
object. The @emph{expander} uses a suite of rewriting rules to elaborate
this syntax object into Racket's kernel syntax. This latter phase
conceptually employs a tower of languages, and the elaboration gradually
turns a program of one level into a language of the next lower level. In
reality, the layers of this tower are not separated through sharp
boundaries, and the intermediate programs may never exist in a pure form. 

@figure-here["fig:mj-impl" "Structure of the MiniJava implemntation"]{
 @pipeline-diagram}

@Figure-ref{fig:mj-impl} presents the overall pipeline of the MiniJava
implementation in Racket. Step 1 turns the Java-like syntax into a syntax
object, a combination of the symbolic source program and syntax properties;
this object roughly corresponds to an abstract syntax tree (AST).  Step 2
elaborates this AST into a prefix variant of MiniJava in a
conventional manner. The choice of type elaboration over checking allows
the injection of type annotations that help implement efficient method
calls. The prefix variant of MiniJava is an untyped, parenthesized version
of MiniJava. 

Once a MiniJava program has been elaborated into prefix form, the
regular expansion process takes over. Step 3 indicates how parenthesize
MiniJava programs are rewritten into plain @tt{#lang racket}
constructs. This is articulated with a (relatively small) suite of
rewriting rules that map classes, method calls, and so on into functional
Racket constructs. 

As indicated in the preceding section, the implementation of @tt{minijava}
consists of a reader module, which implements steps 1 and 2, and a language
module, which implements the syntactic translation. The former employs
Racket's lexing and parsing libraries. While lexing and parsing libraries
require no explanation, the type checking needs some discussion
(@secref{sub:lang}).  The language module consists of syntax rewriting
rules and any functions in the target code that Racket does not provide
already (@secref{macros}, but also @secref{comm}). Both of these modules
are implemented in the ordinary @tt{racket} language.

Finally, step 4 indicates that the existing Racket language elaborates the
program into core Racket. The latter is known as @tt{#%kernel}. This step
is well-established and does not deserve any attention. 

In contrast, the integration of @tt{#lang minijava} with DrRacket deserves
a thorough explanation (@secref{sub:drracket}). Essentially, the pipeline
of @figure-ref{fig:mj-impl} preserves essential properties across the
various transformations. In turn, DrRacket implements a protocol between
the expanded syntax and its editor, which plug-in tools can exploit to
implement an ecosystem for a language such as MiniJava. 

@; -----------------------------------------------------------------------------
@figure*["fig:typecheck-mod-beg" "Typechecking the abstract syntax tree"]{
 @typecheck-mod-beg}

@section[#:tag "sub:lang"]{@tt{#lang minijava}: Parsing and Type Elaboration}

Racket's @tt{#lang} mechanism wraps the entire content of a module
(everything below the language specification) into a single syntactic
object, called ``module begin.'' A language-implementation module may
therefore export its own module-begin constructor and thus take over the
interpretation of an entire module at once. The elaboration process uses
the rewriting rule for a ``module begin'' to perform module-level
computations. The result of the elaboration must defer to a ``module
begin'' construction of some other language.

@Figure-ref{fig:typecheck-mod-beg} shows how @tt{minijava} exploits this
mechanism. The module defines a ``module begin''
construct---@code{mj-module-begin}. Its export specification says that
@racket[mj-module-begin] becomes the ``module begin'' wrapper for the
@tt{minijava} language. The one from the underlying prefix MiniJava
language remains hidden. 

Our MiniJava @racket[#%module-begin] form proceeds in two steps. First, it
hands the list of prefix class definitions to the auxiliary
@racket[typecheck-program] function. This syntax-level function implements
an ordinary recursive-descent type elaboration mechanism. The result is a
prefix MiniJava program. Second, this result is wrapped into the
imported ``module begin'' mechanism from the prefix MiniJava
implementation. This latter turns out to be Racket's ``module begin''
mechanism, meaning the Racket elaborator takes over the 
rest of the compilation pipeline. By linguistic reuse, MiniJava variables
become Racket variables, MiniJava conditionals become Racket conditionals,
and only forms without Racket pendant synthesize substantially new code. 

@; -----------------------------------------------------------------------------
@section[#:tag "macros"]{@tt{#lang minnijava}: Language Constructs}

One such form is MiniJava's @tt{while} construct. A use of it appears on
line 7 in the left half of @figure-ref{expansion}. The corresponding code
in the right column of the same figure shows the code that the Racket
elaborator synthesizes. It uses the relatively simple rewriting rule from
@figure-ref{mj-while-macro} to affect this translation. 

@(figure-here
  "mj-while-macro"
  "Definition of while in prefix MiniJava"
  @mj-while-macro)

The implementation of MiniJava's @racket[while], in
@figure-ref{mj-while-macro}, uses Racket's @racket[define-syntax] form to
bind @racket[while] to a @emph{transformer function} that implements the
compilation step for @racket[while] expressions.  Transformer functions
consume and produce syntax; they are invoked by the expander during the
macro expansion process.  It is important to note that while
@racket[define-syntax] is a special kind of binding form, it is not used to
define macros only. The expander specially recognizes that @racket[while]
is bound to a function and that is what triggers the macro expansion of
@racket[while] forms, but @racket[define-syntax] can bind variable to other
values too (we exploit this power in the next section to handle
@racket[define-class]).  The @racket[while] transformer function's
implementation uses @racket[syntax-parse], a powerful pattern matcher for
defining syntactic extensions@~cite[fortifying-macros].  This macro
contains a single pattern, @racket[(while test body ...)], which indicates
that it expects to see the identifier @racket[while] followed by a test
expression and any number of body expressions.  The pattern matcher binds
@racket[test] and @racket[body] for use within syntax templates,
e.g. expressions contained within @racket[#'⋯] (or
@racket[(@#,racket[syntax] ⋯)]).  The @racket[syntax] form is similar to
the @racket[quote] form that Racket inherits from Lisp, with the key
difference that the former produces a syntax object instead of a datum, and
supports interpolation.  This interpolation feature is used in
@racket[while]'s implementation to support copying pattern variables into a
syntax template.

@(figure*
  "expansion"
  "The Runner class in prefix-parenthesized MiniJava and its Racket expansion"
  expansion-figure)

@Figure-ref{expansion} shows the prefix version of the @racket[Runner] class from
@figure-ref{fig:mj-syntax} and it's expansion into Racket.
The @racket[while] form on the left of @figure-ref{expansion} expands into the @racket[letrec] expression,
spanning lines 7 through 25 on the right.
As the definition of @racket[while] specifies, the loop's condition becomes the guard to Racket's @racket[when]
and the body is copied into the body of @racket[when] before further expansion occurs.

The @racket[while] macro exemplifies the importance of hygiene. The macro
relies on hygienic expansion to prevent the @racket[letrec]-bound variable,
@racket[loop], from conflicting with uses of identically-named variables in
the source syntax of @racket[while] forms. Hygiene eases the job of macro
writers, allowing them to write macros without worrying that their bindings
will conflict with those that appear at macro use sites. In addition to
supporting local variables with hygiene, Racket's expansion process also
ensures that free variables in the syntax object in the template
(@racket[letrec], @racket[λ], and @racket[when] in this case) refer to the
bindings in scope at the definition of the macro, not at the use of the
macro.

@;; while in Java to while in Racket ..
@;; discuss the implementation of the prefix while form (without the syntax parameter mess)
@;; this leaves an example to return to later on
@;; reuse racket variable binding or conditional for the MiniJava forms (linguistic reuse)

@; -----------------------------------------------------------------------------
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

In the previous subsection, we use Racket's @racket[define-syntax] form to
bind @racket[while] to a transformer function. In general, however,
@racket[define-syntax] can bind identifiers to arbitrary values, which are
then available at expansion time.

@(figure
  "mj-new"
  "The implementation of new in prefix MiniJava"
  @mj-new)

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

This technique highlights the distinction between Racket's run-time and compile-time phases.
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

@; -----------------------------------------------------------------------------
@section[#:tag "sub:drracket"]{DrRacket Integration}

Transformer bindings are but one of the communication mechanisms available to
Racket macros.

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


