#lang racket

(require "if-refactor.rkt")

#;(let ([x #t])
  (+ 1 (if x 2 3)))

(+ (let-syntax ([x (λ (stx) #'#t)])
     (if x
         1
         2)) 1)