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

@title[#:tag "sec:conclusion"]{Conclusion}

This paper introduces the key features that make Racket a language
workbench.  Through linguistic dispatch and its rich hygienic macro system,
Racket allows programmers to easily build new languages and extend existing
ones.  Our MiniJava implementation demonstrates language building the
Racket way.  Our solutions to three benchmark problems showcase how to
extend Racket-based languages with new concrete syntax, additional
restrictions, and custom tooling.

Racket's language-building facilities are the result of a gradual evolution
over its twenty year history: starting from Lisp's macro system; adding
hygiene to preserve lexical scope; introducing syntax objects to
encapsulate hygiene, then extending those to carry arbitrary
meta-information; and finally integrating with Racket's module system to
enable reader customization and linguistic dispatch.  Throughout, Racket's
guiding principle of generalizing language features---lexical scope,
modules, etc.---and giving programmers full access to them---on equal
footing with Racket's authors---led us to a powerful tool for building and
extending languages@~cite[manifesto].
