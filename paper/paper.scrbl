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

Racket espouses the view that full-fledged problem solving almost always
calls for language design. In support of this view, it implements a
notion of linguistic reuse, which allows programmers to rapidly develop
and deploy new programming languages. Together with DrRacket, its IDE,
the Racket ecosystem thus makes up a true language workbench. 

This paper demonstrates Racket's capabilities with an implementation of
the the 2016 Language Workbench Challenge. Building on a concise
implementation of MiniJava, it shows how it is easy it is to add new
notation, constrain constructs, and create IDE tools.

}

@include-section{intro.scrbl}
@include-section{racket.scrbl}
@include-section{minijava.scrbl}
@include-section{notation.scrbl}
@include-section{evolution.scrbl}
@include-section{editing.scrbl}
@include-section{conclusion.scrbl}

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

@(generate-bibliography)
