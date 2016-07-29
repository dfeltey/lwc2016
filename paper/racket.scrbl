#lang scribble/sigplan @10pt

@(require pict/code
   (only-in scribble/manual defterm)
          (only-in pict bitmap scale)
          "setup.rkt"
          "bib.rkt"
          "mj-examples.rkt"
          scriblib/figure
          (only-in scribble/core style) scribble/latex-properties
          (only-in scribble/manual racket racketblock hash-lang)
          (only-in racket/format ~a))

@title[#:tag "racket-lwc"]{The Racket Language Workbench}

@;; Sections
@;; - #lang/module-begin talk about the pipeline and typechecking
@;;   show the typechecking module-begin impl
@;; - expand the program to use the even class in a main class (use while and new)
@;; - demonstrate syntactic extension with MiniJava's while macro
@;; - talk about compile-time information/define-syntax with the implementation of new
@;; Then the refactoring and break sections can talk about syntax properties and syntax parameters

Racket promotes a language-oriented view of problem solving. To this end,
it enables programmers to quickly build many different languages so that
they can solve each aspect on its own linguistic terms. Indeed, the first
line of every single module in a Racket application must specify its
implementation language. 

To support this language-development idiom, Racket fully embraces the
idea of linguistic reuse@~cite[sk-dissertation]. According to this view,
the development of a new language consists of adding, subtracting, and
re-interpreting constructs and run-time facilities from a base language.
Indeed, even the installation of a new language takes place within the
Racket ecosystem. Basically a language is a Racket component that provides
certain services; a module's language specification (the first line) simply
points to a component that implements a language@~cite[you-want-it-when].

A plain Racket module that exports certain construct is the simplest
language component. A sophisticated variant consists of modules that
implement a reader---a lexer and parser for any imaginable unicode-based
notation---and a semantics module. In principle, both of these modules have
complete control over the body of a client module. Conventions dictate a
certain organization, however.

In principle, a programmer can implement a Racket language in several
different ways: 
@;
@itemlist[

@item{as a plain interpreter that repeatedly traverses the code of a client
program;}

@item{as a compiler that maps a client program to a target language, either
within or outside of the Racket ecosystem;} 

@; cite DVH's paper
@item{as a Redex@~cite[redex-book] reduction semantics; or}

@item{as a linguistic derivative of an existing Racket language.}

]
@;
Racket strongly encourages this last approach, because it delivers results
more quickly than the others. But, all of these approaches are useful in
certain situations, and on occasion, as in the case of MiniJava, an
implementation may borrow elements from several approaches.

Deriving one language from another means creating a translation of new
linguistic constructs into those of the ``parent'' language and a run-time
library. By transitivity, all other elements of the run-time system (the
vm, the jit compiler, the garbage collector, etc.) are inherited from the
primitive core of Racket. We refer to the syntactic aspect as
@defterm{syntax elaboration} and consider it the critical part of language
derivation. 

Technically, the derivation works as follows. A language module may export
a subset of the constructs and functions of some base language, which
implicitly subtracts features from a language; it may export additional
features and functions, which adds new capabilities; and it may
re-interpret existing features, say, function application or @racket[if]
statements.  The re-interpretation is accomplished by defining a new
construct or function in a module and exporting it under the name of a
feature that already exists in the base language. 

A Racket programmer uses the @defterm{syntax object system} to create new
linguistic constructs. From far enough away, this system is the
great-grandson of Scheme and Lisp's hygienic macro system@~cite[lisp-macros
hygienic-macros]. The system represents syntactic terms via syntax object,
which include properties of the source syntax as well as those specified by
a language implementor @~cite[syntactic-abstraction-in-scheme].

Like the Lisp macro system of lore, Racket's syntax object system allows
the specification of rewriting rules on syntax objects. Unlike Lisp or
Scheme macros, Racket's rewriting rules provide sophisticated services. For
example, they automatically propagate source information so that they can
report error in terms of the original @emph{source notation}. Similarly,
the rules almost automatically enforce context-free and some
context-sensitive constraints so that error messages use the
@emph{concepts} of the surface language@~cite[fortifying-macros]. Lastly,
these rewriting rules can also articulate transformations on complete
modules and on existing linguistic constructs---giving them the expressive
power to track context-sensitive modules and to assign new meaning to old
words. The implementation of MiniJava illustrates all these ideas (and
more) quite well, and it is time to switch from abstract explanations to
concrete examples.
