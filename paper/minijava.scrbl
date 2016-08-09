#lang scribble/sigplan @10pt

@(require pict/code
          (only-in pict bitmap scale)
          "setup.rkt"
          "bib.rkt"
          "mj-examples.rkt"
          scriblib/figure scriblib/footnote
          (only-in scribble/core style) scribble/latex-properties
          (only-in scribble/manual racket racketblock hash-lang)
          (only-in racket/format ~a))

@title[#:tag "sec:minijava"]{MiniJava via Racket}

The Racket language proper consists of a module-oriented, untyped
functional language in the spirit of Lisp and Scheme. In contrast, MiniJava
(@secref{sub:minijava}) is a class-based, object-oriented
programming language in the spirit of Java. Using Racket's syntax system,
it is nevertheless possible to create a realistic implementation of
MiniJava with a relatively small effort---thanks to linguistic reuse.

This section provides an overview of the MiniJava implementation
(@secref{sub:overview}) followed by a presentation of the individual
building blocks. Where necessary, the section also explains Racket features
on a technical basis.

@figure-here["fig:mj-syntax" "A sample MiniJava program"]{
 @mj-simple-example}

@; -----------------------------------------------------------------------------
@section[#:tag "sub:minijava"]{MiniJava}

@Figure-ref{fig:mj-syntax} displays a MiniJava program. Like any Java
program, it consists of a series of class definitions, one of them
designated as the ``main'' class. Classes have public methods
and fields. Each of these comes with a type signature, where types are
the names of classes plus the usual primitive types (e.g., @tt{int}). The
body of a method may use the familiar statements of an imperative language:
assignments, conditionals, and loops. MiniJava expressions are also the
familiar ones.

MiniJava lacks many of Java's sophisticated features: abstract
classes, interfaces, packages, etc. The goal is to help students in
introductory courses, not to model the complexities of a real-world
language.

@; -----------------------------------------------------------------------------
@section[#:tag "sub:overview"]{From MiniJava to Core Racket: an Overview}

Whereas a typical compiler's front end parses a textual program into an
abstract syntax tree (AST), an incremental Racket implementation separates
this process into two distinct steps: the reader and the expander. The
@emph{reader} turns a textual program into a syntax object. The
@emph{expander} uses a suite of rewriting rules to elaborate this syntax
object into Racket's kernel syntax. This latter phase conceptually employs
a tower of languages, and the elaboration gradually turns a program of one
level into a language of the next lower level. In reality, the layers of
this tower are not separated through sharp boundaries, and the intermediate
programs may never exist in a pure form.

@Figure-ref{fig:mj-impl} presents the overall pipeline of the MiniJava
implementation in Racket. Step 1 turns the Java-like syntax into a syntax
object, a combination of the symbolic source program and syntax properties;
this object roughly corresponds to an abstract syntax tree.  Step 2
elaborates this AST into a prefix variant of MiniJava in a conventional
manner. The choice of type elaboration over checking allows the injection
of type annotations that help implement efficient method
calls@~cite[classes-and-mixins]. The prefix variant of MiniJava is an
untyped, parenthesized version of MiniJava.

@figure-here["fig:mj-impl" "Structure of the MiniJava implementation"]{
 @pipeline-diagram}

Once a MiniJava program has been elaborated into prefix form, the
regular expansion process takes over. Step 3 indicates how parenthesized
MiniJava programs are rewritten into plain @tt{#lang} @tt{racket}
constructs. This transformation is articulated with a (relatively small) suite of
rewriting rules that map classes, method calls, and so on into functional
Racket constructs. 

As mentioned in the preceding section, the implementation of @tt{mini-java}
consists of a reader module, which implements steps 1 and 2, and a language
module, which implements the syntactic translation of step 3. The former
employs Racket's lexing and parsing libraries. While lexing and parsing
libraries require no explanation, the type elaboration needs some discussion
(@secref{sub:lang}).  The language module consists of syntax rewriting
rules and any functions in the target code that Racket does not provide
already (@secref{macros}, but also @secref{comm}). Both of these modules
are implemented in the ordinary @tt{racket} language.

Finally, step 4 indicates that the existing Racket language elaborates the
program into core Racket. The latter is known as @tt{#%kernel}. This step
is well-established and does not deserve any attention. 

In contrast, the integration of @tt{#lang mini-java} with DrRacket deserves
a thorough explanation (@secref{sub:drracket}). Essentially, the pipeline
of @figure-ref{fig:mj-impl} preserves essential properties across the
various transformations. In turn, DrRacket implements a protocol between
the expanded syntax and its editor, which plug-in tools can exploit to
implement an ecosystem for a language such as MiniJava. 

@; -----------------------------------------------------------------------------
@figure*["fig:typecheck-mod-beg" "Typechecking the abstract syntax tree"]{
 @typecheck-mod-beg}

@section[#:tag "sub:lang"]{@tt{#lang mini-java}: Parsing and Type Elaboration}

Racket's @tt{#lang} reader mechanism wraps the entire content of a module
(everything below the language specification) into a single syntactic
object, a @racket[#%module-begin] form. A language-implementation module may
therefore opt in to linguistic dispatch by exporting its own @racket[#%module-begin] macro and thus take over the
interpretation of an entire module at once.  The result of a
@racket[#%module-begin] expansion must be a @racket[#%module-begin] form
in some other language, usually the base language. 

@Figure-ref{fig:typecheck-mod-beg} shows how @tt{mini-java} exploits this
mechanism. The module defines a ``module begin''
construct as @racket[mj-module-begin]. The module's export specification says that
@racket[mj-module-begin] becomes the @racket[#%module-begin] form for the
@tt{mini-java} language. The implementation of @racket[mj-module-begin]
expands to a @racket[#%module-begin] that (through an import not shown in the
figure) implements our prefix variant of MiniJava,
but that @racket[#%module-begin] is hidden from a module that is implemented
in the surface @tt{mini-java} language, which instead sees @racket[mj-module-begin].

Before expanding to an underlying @racket[#%module-begin] form, @racket[mj-module-begin]
hands the list of class definitions (ASTs) to the auxiliary
@racket[typecheck-program] function. This syntax-level function implements
an ordinary recursive-descent type elaboration mechanism on a MiniJava variant
using infix operations, and the result is a
MiniJava program using prefix forms.

The @racket[#%module-begin] for prefix MiniJava forms turns out to be Racket's usual
@racket[#%module-begin] form, so Racket's usual macro expansion takes over the
rest of the compilation pipeline. By linguistic reuse, MiniJava variables
become Racket variables, MiniJava conditionals become Racket conditionals,
and only forms without Racket analogs synthesize substantially new code.

@; -----------------------------------------------------------------------------
@section[#:tag "macros"]{@tt{#lang mini-java}: Language Constructs}

One form without a Racket precedent is MiniJava's @racket[while] construct.
A use of the construct appears on
line 7 in the left half of @figure-ref{expansion}. The corresponding code
in the right column of the same figure shows the code that is synthesized
by expansion. Racket's expander uses the relatively simple rewriting rule from
@figure-ref{mj-while-macro} to effect this translation. 

@(figure-here
  "mj-while-macro"
  "Definition of while in prefix MiniJava"
  @mj-while-macro)

The implementation of MiniJava's @racket[while] uses Racket's @racket[define-syntax] form to
bind @racket[while] to a @emph{transformer function}. The latter implements
the compilation step for @racket[while] loops; the definition informs
the macro expander that whenever a @racket[while] AST node shows up, it must
invoke the transformer on the node.

The definition of  the @racket[while] transformer function uses
@racket[syntax-parse], a powerful pattern matcher for defining syntactic
extensions@~cite[fortifying-macros].  This macro contains a single pattern,
@racketblock[(while test:expr body ...)] which indicates that it expects
to see @racket[while] followed by a @racket[test] expression
and any number of @racket[body] elements. The constraint @racket[:expr]
forces @racket[test] to be an expression, while @racket[body] could be
either a definition or an expression. If the expression constraint is violated,
@racket[syntax-parse] automatically synthesizes an error message in terms
of source notation and source concepts. 

The pattern matcher binds @racket[test] and @racket[body] for use within
the code-generation template. Such a template is specified with  @racket[#`⋯] or
@;
@racketblock[(@#,racket[quasisyntax/loc] ⋯)]
@; 
This form resembles the @racket[quasiquote] form that Racket inherits from
Lisp. They differ in that @racket[quasisyntax/loc] produces a syntax object
instead of an S-expression and that it supports automatic interpolation. With
interpolation,  macro expansion splices the values of
pattern variables into the template---indeed, this is what makes the
expression a template. The @racket[/loc] part means that it also moves
along source-location information. 

@(figure*
  "expansion"
  "The Runner class in prefix-parenthesized MiniJava and its Racket expansion"
  expansion-figure)

@Figure-ref{expansion} shows the prefix version of the @racket[Runner] class from
@figure-ref{fig:mj-syntax} and its expansion into Racket.
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

@; -----------------------------------------------------------------------------
@section[#:tag "comm"]{Inter-macro Communication}

Isolated macro definitions do not suffice to transform MiniJava into
Racket. Consider a @racket[new] expression, which instantiates a class.
The name of the class is not enough to create the object. The
construction of an object also needs to know how many slots to allocate for fields
and how to connect with the ``vtable,'' i.e., the method dispatch table of
the class. In short, the Racket form that defines a MiniJava class must
@emph{communicate} with the Racket form that implements a @racket[new]
expression. Racket builds on ordinary lexical scope to provide a communication
channel for distinct macros for just this purpose@~cite[mtwt].

To make this idea concrete, consider line 5 of
@figure-ref{expansion}. It shows how our implementation of MiniJava
expands
@;
@racketblock[(new Parity)] 
@; 
from the left-hand column into 
@;
@racketblock[(Parity:constructor)]
@;
in the right-hand column, which is a call to the constructor for the @racket[Parity]
class. For this translation, the @racket[new] macro must identify the
constructor function for the @racket[Parity] class---and avoid all possible
interference from other definitions.

@; -----------------------------------------------------------------------------
@figure-here[ "mj-new" "The implementation of new in prefix MiniJava"]{
  @mj-new}

Our implementation accomplishes this communication with the seemingly simple, but rather
unusual syntax transformer of @figure-ref{mj-new}. At first glance, this
definition looks just like the one for @racket[while] from
@figure-ref{mj-while-macro}. The @racket[syntax-parse] form specifies an input
pattern that consists of @tt{new} and an identifier that
specifies a class. The template constructs uses @racket[#:escape + [ ⋯] ]
to construct an application, using brackets for emphasis instead of plain
parentheses.@note{Racket uses @tt{[}...@tt{]} and @tt{(}...@tt{)}
interchangeably.} A close look now reveals an unusual concept, however,
specifically the @racket[#:escape + #,⋯]
or
@;
@racketblock[(@#,racket[unsyntax] ⋯)]
@; 
form. It escapes from the template, constructs code at compile time via an
arbitrary computation, and inserts that code into the template in lieu of
itself. The question is what this computation affects and how it works. 

To this end, we turn our attention to the expansion of the @racket[Parity]
class from our running example. It is shown in @figure-ref{expansion}
at line 40.  MiniJava forms are compiled away to ordinary Racket
code, which uses vectors to represent objects and method
tables. Critically, though, the @racket[define-class] form for the
@racket[Parity] class expands to three definitions: 
@;
@itemlist[

@item{the run-time method table shared by all @racket[Parity] instances,}

@item{the constructor that creates instances, and}

@item{a syntax definition of compile-time information about the
@racket[Parity] class used to guide the expansion of other forms that refer
to @racket[Parity].}

]

The first definition, @racket[Parity:runtime-method-table]
(@figure-ref{expansion} line 40), is a vector storing the methods
@racket[is_odd] and @racket[is_even].  The second one,
@racket[Parity:constructor] on line 53, is a function of no arguments for
creating new instances of the @racket[Parity] class. Because the
@racket[Parity] class has no fields, its instance vectors contain only a
reference to the method table.  The third definition, that of
@racket[Parity] on line 56, uses @racket[define-syntax], but in a rather
surprising and unusual manner. 

While the preceding section employs @racket[define-syntax] to bind
syntax-transforming functions to a name,@note{The syntax system specially
recognizes that @racket[while] is bound to a function.} here it binds a
variable to an ordinary value, specifically, a record. The record
constructor, @racket[static-class-info], is a function of five values. Its
instances thus store compile-time information about the class: an
identifier bound to the parent class information (or @racket[#f] if there
is no parent class); a table mapping method names to vector offsets; a
syntax object referring to the run-time method table; a syntax object that
points to the constructor; and a count of the fields in the class.

Now that the variable @racket[Parity] is compile-time bound to information, other macro
computations may retrieve this information. Technically, these macros must
use the @racket[syntax-local-value] procedure for this purpose. And that
explains the inner expression in the template of @racket[new] in
@figure-ref{mj-new}. It is passed a syntax object that points to a class
identifier, and @racket[syntax-local-value] uses this identifier to
retrieve the @racket[static-class-info] record.  Next, the function
@racket[static-class-info-constructor-id] is simply a field accessor (which
would be written as a ``@tt{.constructor_id}'' suffix in infix-notation
languages) that returns the value of the second-to-last field in the
record. In this case, the value is the identifier
@racket[#'Parity:constructor].

A reader may wonder why the @racket[new] macro does not just synthesize
the name of the object constructor directly from the name of the given
class. Doing so looks natural, but it may interfere with intermediate
re-bindings of the class name. Splicing syntax objects into the syntax
object for the template guarantees hygienic code synthesis and thus
automatically creates correctly scoped code. 

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

This technique highlights the distinction between Racket's run-time and
compile-time phases.  The @racket[new] and @racket[send] macros must call
@racket[syntax-local-value] at compile-time to access static class
information.  In general, this means that arbitrary, possibly even
side-effecting, code may need to run at compile-time to expand a syntactic
form. Racket addresses the issue of mingling compile-time code with
run-time code through a phase distinction@~cite[you-want-it-when] that
makes explicit the execution time of a piece of code. 

@; -----------------------------------------------------------------------------
@section[#:tag "sub:drracket"]{DrRacket Integration}

Syntax bindings are but one of the communication mechanisms available to
Racket macros. Syntax properties provide another one. Recall that
Racket's syntax objects are data structures that include symbolic
representations of program fragments as well as additional syntax
properties.  Macros may attach additional key-value pairs to syntax
objects.  Just as syntax bindings allow communication between distinct
macros, syntax properties open a communication channel between different
processing passes, including external tools@~cite[langs-as-libs].

@; -----------------------------------------------------------------------------
@figure-here[ "tool-tips" "MiniJava type tool-tips in DrRacket"]{
 @(scale (bitmap "type-tool-tips-cursor.png") .6)}


DrRacket's @emph{check syntax} tool exploits this information to provide a better user experience
when editing source code. It draws arrows between binding and bound occurrences of
variables and it renders information in tooltips. To get the binding information
and tooltip information, it consults syntax properties, as well as just using the
underlying lexical information in the fully expanded program.

To illustrate the idea, our implementation of MiniJava
attaches type information to syntax objects via syntax
properties. In the example shown in @figure-ref{tool-tips},
programmers can see the type information via tool tips (@tt{Parity} in this case),
and see binding information via arrows connecting identifiers.

Some MiniJava variables become Racket variables in the fully expanded
program, e.g., the parameter @racket[n] of the @racket[run]
method. Others, like @racket[check], are fields and are
compiled into vector references.
@Figure-ref{expansion} shows this
explicitly: the definition of the @racket[check] field on
line 2 is absent from the expanded program and the reference
to @racket[check] on line 9 compiles into the expression 
@racket[(vector-ref this 1)] on line 11 of the expanded
program. DrRacket's @emph{check syntax} tool nonetheless
infers the correct binding structure for such variables
and displays binding arrows accordingly, as @figure-ref{tool-tips} shows.


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


