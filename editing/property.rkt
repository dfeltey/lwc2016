#lang racket

(provide add-refactor-property get-refactor-property)

(define refactor-property-key 'refactor)
(define (add-refactor-property stx val)
  (syntax-property stx refactor-property-key val))
(define (get-refactor-property stx)
  (syntax-property stx refactor-property-key))