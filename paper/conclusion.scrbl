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

This paper introduces the key elements of the Racket language workbench via
the MiniJava sample language and three benchmark challenges. Racket's
linguistic reuse capabilities allows programmers to easily build new
languages and then extend and adapt them.  Indeed, Racket's unique approach
to linguistic reuse extends to its ecosystem.

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
