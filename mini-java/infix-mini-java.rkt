#lang racket

(require (for-syntax "typecheck.rkt"
                     syntax/parse)
         (submod "typecheck.rkt" literals)
         "prefix-mini-java.rkt")

(provide (all-from-out (submod "typecheck.rkt" literals))
         (except-out (all-from-out "prefix-mini-java.rkt") #%module-begin)
         (rename-out [module-begin #%module-begin]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ class ...)
     (define post-typechecking (typecheck-program #'(class ...)))
     (quasisyntax/loc stx
       (#%module-begin #,@post-typechecking))]))
