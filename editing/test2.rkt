#lang racket

(require "if-refactor.rkt")

(let ([x #t])
  (+ 1 (if x 2 3)))