#lang racket

(require parser-tools/lex
         syntax/readerr
         racket/syntax)

(provide (all-defined-out))

(define-empty-tokens Operators
  (OR && == < + - * % ! =))

(define-empty-tokens Separators
  (O_PAREN C_PAREN O_BRACE C_BRACE O_BRACKET C_BRACKET SEMI_COLON PERIOD COMMA LANG White-Space))

(define-empty-tokens EmptyLiterals (TRUE_LIT FALSE_LIT BREAK_LIT EOF))

(define-empty-tokens Keywords 
  (if while else this new int boolean public static void main String class extends return System.out.println length super))

(define-tokens mini-java-vals
  (INTEGER_LIT IDENTIFIER))

(define-tokens special-toks (2D))