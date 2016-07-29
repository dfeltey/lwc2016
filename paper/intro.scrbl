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

@title[#:tag "intro"]{The Racket Manifesto in a Nutshell}

Racket really is a programming-language programming
language@~cite[manifesto]. It provides both unique linguistic
support@~cite[plt-tr1] for the rapid prototyping of languages as well as an
ecosystem with unmatched elements@~cite[drracket]. As such, it is a first
step toward the idea of a language workbench as originally imagined by Bill
Scherlis and Dana Scott in the early 1980s.

After explaining the linguistic elements of language development in Racket
(@secref{racket-lwc}), this paper illustrates Racket as a language
workbench starting with (@secref{sec:minijava}) an implementation of
MiniJava@~cite[mini-java].

Based on the MiniJava implementation, we present the results of tackling
three challenge benchmarks from the 2016 language workbench
challenge. First, we add a notation to MiniJava so that programmers can
express finite-state machines as unicode-based tables within their code
(@secref{notation}). Second, we demonstrate how little effort it takes to
add a @racket[break] construct to MiniJava and to enforce that is is valid
only within a @racket[while] loop (@secref{evolution}). Third, we explain
how to connect an implemented language such as MiniJava with DrRacket, a
part of the ecosystem. Specifically, we show how easy it is to add tools
for program restructuring, chosen from the @emph{Editing} category
(@secref{editing}).
