#lang racket

(require (prefix-in r: racket)
         syntax/parse/define
         racket/stxparam
         (for-syntax syntax/parse
                     syntax/id-table
                     racket/stxparam-exptime
                     racket/dict
                     racket/match
                     racket/syntax
                     (only-in racket/sequence in-syntax)
                     (prefix-in r: racket)
                     "../editing/syntax-info.rkt"
                     "../editing/property.rkt"))

(provide (all-defined-out)
         #%module-begin #%app
         #%datum true false < + - *
         (rename-out [begin         compound]
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

 ;; might also need the field names ...
 (struct static-class-info (super-class
                            compile-time-method-table
                            run-time-method-table-id
                            constructor-id))

 )

;; can't just expand to `begin`; needs to be run after everything else
(define-syntax (main stx)
  (syntax-parse stx
    [(_ body ...)
     (syntax-local-lift-module #`(module* main #f (main-method)))
     #'(define (main-method)
         body ...)]))

;; TODO add inheritance, and super. and whatever else old version had
(define-syntax (define-class stx)
  (syntax-parse stx
    [(_ name (~optional (~seq #:extends super-class) #:defaults ([super-class #f]))
        var:var-decl ... meth:meth-decl ...)
     (define run-time-method-table-id (generate-temporary 'runtime-method-table))
     (define has-super? (attribute super-class))
     (define super-static-info (and has-super? (syntax-local-value #'super-class)))
     (define super-method-table
       (and has-super? (static-class-info-run-time-method-table-id super-static-info)))
     (define super-compile-time-table
       (if has-super?
           (static-class-info-compile-time-method-table super-static-info)
           (make-immutable-free-id-table)))
     (define super-methods
       (map car (sort (dict->list super-compile-time-table) < #:key cdr)))
     (define method-mapping
       (make-immutable-free-id-table
        (map cons (syntax->list #'(meth.name ...)) (syntax->list #'(meth ...)))))
     (define methods
       (append
        (for/list ([meth-name (in-list super-methods)]
                   [i (in-naturals)])
          (cons meth-name (dict-ref method-mapping meth-name i)))
        (for/list ([method (in-syntax #'(meth ...))]
                   [meth-name (in-syntax #'(meth.name ...))]
                   #:unless (dict-ref super-compile-time-table meth-name #f))
          (cons meth-name method))))
     (define/with-syntax super (if has-super? #'#'super-class #'#f))
     (define/with-syntax (run-time-method ...)
       (for/list ([meth (in-list methods)])
         (match-define (cons name body-or-index) meth)
         (cond
           [(number? body-or-index) #`(vector-ref #,super-method-table #,body-or-index)]
           ;; name is used here to support super calls
           [else #`(method #,body-or-index)])))
     #`(begin
         (define #,run-time-method-table-id
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
             (vector run-time-method ...)))
         (define (constructor)
           (vector #,run-time-method-table-id
                   #,@(r:make-list (r:length (syntax->list #'(var.name ...))) #f)))
         (define-syntax name
           (static-class-info
            super
            ;; compile-time method table
            #,(for/fold ([t (make-immutable-free-id-table)])
                  ([meth-name (in-list (map car methods))]
                   [i         (in-naturals)])
                (dict-set t meth-name i))
            #'#,run-time-method-table-id
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

(define-syntax (if stx)
  (syntax-parse stx
    [(_ test then else)
     (add-refactor-property
      (syntax/loc this-syntax (r:if test then else))
      (list (if (syntax-property stx 'mini-java) 'mini-java 'sexp-mini-java)
            (syntax-loc stx)
            (syntax-loc #'test)
            (syntax-loc #'then)
            (syntax-loc #'else)))]))

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
