#lang racket/base

(module reader syntax/module-reader
  mini-java/infix-mini-java
  #:read-syntax
  (λ (name in)
    (read-syntax name in))
  #:read
  (λ (in)
    (map syntax->datum (read-syntax 'prog in)))
  #:whole-body-readers?
  #t
  
  #:info
  (lambda (mode default get-default)
    (get-info mode default get-default))
  
  (require racket
           "lexer-sig.rkt"
           "parser-sig.rkt"
           "lexer-unit.rkt"
           "parser-unit.rkt"
           2d/lexer)
  
  (define-compound-unit/infer lexer+parser@
    (import)
    (export lexer^ parser^)
    (link lexer@ parser@))
  
  (define-values/invoke-unit/infer lexer+parser@)
  
  (define (get-info mode default get-default)
    (case mode
      [(color-lexer) (2d-lexer color-lexer)]
      [(drracket:indentation)
       (dynamic-require 'scribble/private/indentation 'determine-spaces)]
      [else (get-default mode default)]))
  
  (define (read-syntax name in) (parse in name 'program)))
