#lang racket

(require (prefix-in r: racket)
         syntax/parse/define
         racket/stxparam
         (for-syntax syntax/parse
                     syntax/id-table
                     racket/stxparam-exptime
                     racket/dict
                     (prefix-in r: racket)))

(provide (all-defined-out)
         #%module-begin #%app
         #%datum true false < + - *
         (rename-out [begin         compound]
                     [begin         main]
                     [displayln     System.out.println]
                     [set!          =]
                     [vector-set!   array=]
                     [and           &&]
                     [vector-ref    index]
                     [vector-length length]
                     [not           !]
                     [make-vector   new-int-array]))


(define-syntax-rule (define-literals (lit ...))
  (begin (define-syntax lit (syntax-rules ())) ...))
(define-literals (define-field define-method))

(define-syntax-parameter current-this (syntax-rules ()))
(define-syntax this (lambda (stx) (syntax-parameter-value #'current-this)))

(begin-for-syntax

 (define-syntax-class var-decl
   #:literals (define-field)
   (pattern (define-field type:id name:id)))
 ;; TODO add check-syntax collaboration?

 (define-syntax-class meth-decl
   #:literals (define-field define-local)
   (pattern (define-method ret-type:id name:id ([arg-type:id arg-name:id] ...)
              ;; can have `define-local`s here, but we just consider them part of the body
              body ...)))

 (struct static-class-info (compile-time-method-table run-time-method-table-id constructor-id))

 )

;; TODO add inheritance, and super. and whatever else old version had
(define-syntax (define-class stx)
  (syntax-parse stx
    [(_ name var:var-decl ... meth:meth-decl ...)
     #`(begin
         (define run-time-method-table
           (let-syntax (#,@(for/list ([var-name (in-list (syntax->list #'(var.name ...)))]
                                      [i        (in-naturals 1)]) ; 0 is method table
                             #`[#,var-name
                                (make-set!-transformer
                                 (lambda (stx)
                                   (syntax-parse stx
                                     #:literals (set!)
                                     [(set! x val) #'(vector-set! this #,i val)]
                                     [x            #'(vector-ref this #,i)])))]))
             ;; method table
             (vector (method meth) ...)))
         (define (constructor)
           (vector run-time-method-table
                   #,@(r:make-list (r:length (syntax->list #'(var.name ...))) #f)))
         (define-syntax name
           (static-class-info
            ;; compile-time method table
            #,(for/fold ([t (make-immutable-free-id-table)])
                  ([meth-name (in-list (syntax->list #'(meth.name ...)))]
                   [i         (in-naturals)])
                (dict-set t meth-name i))
            #'run-time-method-table
            #'constructor)))]))

;; for post-processing
;; `define-method` (and `define-field`) is fixed syntax in `define-class`
(define-simple-macro (method meth:meth-decl)
  (lambda (receiver meth.arg-name ...)
    (syntax-parameterize
     ([current-this #'receiver])
     meth.body ...)))

(define-simple-macro (define-local type x)
  (define x #f))

;; statements (rest are just re-exported from Racket, linguistic-reuse-style)

(define-simple-macro (if test then else)
  (r:if test then else)) ; TODO add refactoring prop

(define-simple-macro (while test body ...)
  (let loop ()
    (when test
      body ...)))

;; expressions (except Racket re-exports)

(define-syntax (send stx)
  (syntax-parse stx
    [(_ the-class ; added by typechecking pass
        receiver method-name arg ...)
     #:do [(define ct-method-table
             (static-class-info-compile-time-method-table (syntax-local-value #'the-class)))]
     #`(let* ([rt-method-table (vector-ref receiver 0)]
              [method-index    #,(dict-ref ct-method-table #'method-name)]
              [method          (vector-ref rt-method-table method-index)])
         (method receiver arg ...))]))

(define-syntax (new stx)
  (syntax-parse stx
    [(_ the-class)
     #`(#,(static-class-info-constructor-id (syntax-local-value #'the-class)))]))
