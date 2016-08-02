#lang info
(define collection 'multi)

(define deps
  '(("2d-lib" #:version "1.1")
    "base"
    "data-lib"
    "drracket-plugin-lib"
    "drracket-tool-lib"
    "gui-lib"
    "parser-tools-lib"
    "pict-lib"
    "rackunit-lib"
    "scribble-lib"
    "syntax-color-lib"))

(define build-deps
  '("at-exp-lib"))
