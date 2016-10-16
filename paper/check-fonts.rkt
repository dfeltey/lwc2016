#lang racket/base
(require racket/gui/base)
(define the-font "Inconsolata with Boxes")
(unless (member the-font (get-face-list))
  (error 'check-fonts.rkt "expected ~a to be installed" the-font))

