#lang racket

(provide (rename-out [:if if]))

(require (for-syntax "property.rkt" syntax/parse "syntax-info.rkt"))


(define-syntax (:if stx)
  (syntax-parse stx
    #:literal-sets (kernel-literals)
    [(_ test then else)
     (add-refactor-property
      (quasisyntax/loc stx
        (if test then else))
      (list (syntax-loc stx)
            (syntax-loc #'test)
            (syntax-loc #'then)
            (syntax-loc #'else)))]))

