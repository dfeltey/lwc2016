#lang racket

(provide add-refactor-property get-refactor-property)

;; ~~~EXTRACT:refactor-prop~~~
(define (add-refactor-property stx val)
  (syntax-property stx 'refactor val))
(define (get-refactor-property stx)
  (syntax-property stx 'refactor))
;; ~~~EXTRACT:refactor-prop~~~