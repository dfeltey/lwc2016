#lang racket

(module reader syntax/module-reader
  mini-java/mini-java
  #:read-syntax
  (λ (name in)
    (read-syntax name in))
  #:read
  (λ (in)
    (map syntax->datum (read-syntax 'prog in)))
  #:whole-body-readers?
  #t

  (require "parser.rkt")
  
  (define (read-syntax name in) (displayln (parse in name))))
