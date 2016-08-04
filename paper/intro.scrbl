#lang scribble/sigplan @10pt

@(require pict/code
          (only-in pict bitmap scale)
          "setup.rkt"
          "bib.rkt"
          "mj-examples.rkt"
          scriblib/figure
          scriblib/gui-eval
          scriblib/footnote
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
three benchmark problems from the 2016 language workbench challenge. First, we
demonstrate a solution to the @emph{Tabular Notation}
problem in the @emph{Editing} category by adding
notation to MiniJava so that programmers can express finite-state machines
as Unicode-based tables (@secref{notation}). Second, we
solve the @emph{Beyond-Grammar Restrictions} benchmark from the
@emph{Evolution and Reuse} category
by showing how to constrain a @racket[break]
construct in MiniJava so that is is valid only within the scope of
@racket[while] (@secref{evolution}). Third, we explain how to connect an
implemented language, such as MiniJava, with DrRacket; specifically, we show
how to add tools for program @emph{Restructuring}, consistent with the
@emph{Editing} benchmark category (@secref{editing}).

Our MiniJava implementation with all of the extensions is available as a Racket package.@note{@url{http://pkgs.racket-lang.org}}
Using the pre-release@note{@url{https://plt.eecs.northwestern.edu/snapshots/}} version
of Racket, run @centered{@tt{raco pkg install lwc2016}} to install it.
To view a selection of sample MiniJava programs and experiment with the language extensions,
select the "Open Require Path…" entry under the "File" menu in DrRacket and type
@tt{lwc2016/examples/programs/}.