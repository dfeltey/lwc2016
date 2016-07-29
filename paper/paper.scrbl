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

@; TODO:
@;; Make sure to list the artifact

@(define extra-tex-code
   (bytes-append #"\\usepackage{pslatex}\n"
                 #"\\usepackage{inconsolata}\n"))

@title[#:style (style #f (list (tex-addition extra-tex-code)))]{
  Languages the Racket Way
  @subtitle{Submission to the 2016 Language Workbench Challenge}
}

@authorinfo["Daniel Feltey" "Northwestern University" "daniel.feltey@eecs.northwestern.edu"]
@authorinfo["Spencer P. Florence" "Northwestern University" "spencer.florence@eecs.northwestern.edu"]
@authorinfo["Tim Knutson" "University of Utah" "tkkemo@gmail.com"]
@authorinfo["Vincent St-Amour" "Northwestern University" "stamourv@eecs.northwestern.edu"]
@authorinfo["Ryan Culpepper" "Northeastern University" "ryanc@ccs.neu.edu"]
@authorinfo["Matthew Flatt" "University of Utah" "mflatt@cs.utah.edu"]
@authorinfo["Robert Bruce Findler" "Northwestern University" "robby@eecs.northwestern.edu"]
@authorinfo["Matthias Felleisen" "Northeastern University" "matthias@ccs.neu.edu"]

@abstract{
Racket provides a wealth of features to assist language designers,
including linguistic dispatch and a powerful syntactic extension system.
To solve the 2016 Language Workbench Challenge, we have implemented a
version of the MiniJava programming language using these key features of
Racket.  Building upon this implementation of MiniJava, we demonstrate the
use of Racket as a language workbench for building and extending languages.
}

@include-section{intro.scrbl}
@include-section{racket.scrbl}
@include-section{minijava.scrbl}
@include-section{notation.scrbl}
@include-section{evolution.scrbl}
@include-section{editing.scrbl}
@section[#:tag "conclusion"]{Conclusion}

@;; Linguistic dispatch (via #lang)
@;; language features as macros
@;; Communication between macros, and between languages and tools
@;;  - syntax properties and define-syntax ....
@;; other cool stuff, readtables, syntax-parameters, DrRacket



@;; cite Fowler's website
@;{Fowler's definition of a language workbench requires:
@itemlist[@item{Free definition of new languages which fully integrate with one another}
          @item{An abstract representation as the primary source of information}
          @item{DSLs defined as a schema, an editor, and generators}
          @item{DSLs can be manipulated with a projectional editor}
          @item{Persisting incomplete or contradictory information in the abstract representation}]
Racket's communicating macros and @tt{#lang} mechanism enable the free definition of new languages.
Syntax objects comprise an abstract representation of programs implemented on top of Racket and can store user specified
information in syntax properties. Racket's @tt{#lang} feature enables the composition of language readers and
bindings which implement the language's semantics, whereas DrRacket's plugin mechanism provides an extensible
editor for manipulating programs that compile down to Racket. Although, DrRacket does not provide a projectional
editing mechanism, its plugin mechanism allows extensions to support program editing. Finally, the syntax object
representation may persist incomplete or contradictory information due to macro expansion introducing syntax from
private modules or rearranging pieces of syntax.}

This paper introduces the key features that make Racket a language workbench.
Through linguistic dispatch and its rich hygienic macro system, Racket allows programmers to easily build new languages and extend existing ones.
Our MiniJava implementation demonstrates language building the Racket way.
Our solutions to three benchmark problems showcase how to extend Racket-based languages with new concrete syntax, additional restrictions, and custom tooling.

Racket's language-building facilities are the result of a gradual evolution over its twenty year history:
starting from Lisp's macro system; adding hygiene to preserve lexical scope;
introducing syntax objects to encapsulate hygiene, then extending those to
carry arbitrary meta-information; and finally integrating with Racket's module
system to enable reader customization and linguistic dispatch.
Throughout, Racket's guiding principle of generalizing language
features---lexical scope, modules, etc.---and giving programmers full access to
them---on equal footing with Racket's authors---led us to a powerful tool for
building and extending languages@~cite[manifesto].

@(generate-bibliography)
