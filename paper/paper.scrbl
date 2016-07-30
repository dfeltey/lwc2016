#lang scribble/sigplan @10pt

@; =================================
@; TODO:
@;; Make sure to list the artifact
@;; I'd much prefer @racket-lang.org 
@; =================================

@(require pict/code
          (only-in pict bitmap scale)
          "setup.rkt"
          "bib.rkt"
          "mj-examples.rkt"
          scriblib/figure
          (only-in scribble/core style) scribble/latex-properties
          (only-in scribble/manual racket racketblock hash-lang)
          (only-in racket/format ~a))

@(define extra-tex-code ;; really? 
   (bytes-append #"\\usepackage{pslatex}\n"
                 #"\\usepackage{inconsolata}\n"))

@title[#:style (style #f (list (tex-addition extra-tex-code)))]{
  Languages the Racket Way
  @subtitle{Submission to the 2016 Language Workbench Challenge}
}

@authorinfo["Daniel Feltey"        "Northwestern University" "daniel.feltey@eecs.northwestern.edu"]
@authorinfo["Spencer P. Florence"  "Northwestern University" "spencer.florence@eecs.northwestern.edu"]
@authorinfo["Tim Knutson"          "University of Utah"      "tkkemo@gmail.com"]
@authorinfo["Vincent St-Amour"     "Northwestern University" "stamourv@eecs.northwestern.edu"]
@authorinfo["Ryan Culpepper"       "Northeastern University" "ryanc@ccs.neu.edu"]
@authorinfo["Matthew Flatt"        "University of Utah"      "mflatt@cs.utah.edu"]
@authorinfo["Robert Bruce Findler" "Northwestern University" "robby@eecs.northwestern.edu"]
@authorinfo["Matthias Felleisen"   "Northeastern University" "matthias@ccs.neu.edu"]

@; -----------------------------------------------------------------------------
@abstract{Racket espouses the view that full-fledged problem solving almost
always calls for language design. In support of this view, it implements a
notion of linguistic reuse, which allows programmers to rapidly develop and
deploy new programming languages. Together with DrRacket, its IDE, the
Racket ecosystem thus makes up a true language workbench.

This paper demonstrates Racket's capabilities with an implementation of the
2016 Language Workbench Challenge. Building on a concise implementation of
MiniJava, it shows how it is easy it is to add new notation, constrain
constructs, and create IDE tools.}

@; -----------------------------------------------------------------------------
@include-section{intro.scrbl}
@include-section{racket.scrbl}
@include-section{minijava.scrbl}
@include-section{notation.scrbl}
@include-section{evolution.scrbl}
@include-section{editing.scrbl}
@include-section{conclusion.scrbl}

@(generate-bibliography)
