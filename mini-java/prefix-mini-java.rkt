#lang racket

(require (prefix-in r: racket)
         syntax/parse/define
         racket/stxparam
         (for-syntax syntax/parse
                     syntax/id-table
                     syntax/id-set
                     racket/stxparam-exptime
                     racket/dict
                     racket/match
                     racket/syntax
                     racket/list
                     (only-in racket/sequence in-syntax)
                     (prefix-in r: racket)
                     "../editing/syntax-info.rkt"
                     "../editing/property.rkt"
                     "state-machine-classes.rkt"))

(provide (all-defined-out)
         2dstate-machine
         #%module-begin #%app
         #%datum true false < + - *
         (rename-out [displayln     System.out.println]
                     [set!          =]
                     [eqv?          ==]
                     [vector-set!   array=]
                     [and           &&]
                     [vector-ref    index]
                     [vector-length length]
                     [not           !]
                     [make-vector   new-int-array]))


(define-syntax-rule (|| x y)
  (or x y))

(define-syntax-rule (compound body ...)
  (begin body ... (void)))

(define-syntax-rule (define-literals (lit ...))
  (begin (define-syntax lit (syntax-rules ())) ...))
(define-literals (define-field define-method))

(define-syntax-parameter this (syntax-rules ()))

(define-syntax-parameter break
  (λ (stx)
    (raise-syntax-error 'break "break used out of context")))

(begin-for-syntax

 (define-syntax-class var-decl
   #:literals (define-field)
   (pattern (define-field name:id)))
 ;; TODO add check-syntax collaboration?

 (define-syntax-class meth-decl
   #:literals (define-field define-local)
   (pattern (define-method name:id (arg-name:id ...)
              ;; can have `define-local`s here, but we just consider them part of the body
              body ...)))

 (define-splicing-syntax-class extends-decl
   #:attributes (super-method-table super-compile-time-table super super-field-count)
   (pattern
    (~seq #:extends super-class:id)
    #:attr super #'#'super-class
    #:attr super-static-info (syntax-local-value #'super-class)
    #:attr super-method-table (static-class-info-run-time-method-table-id (attribute super-static-info))
    #:attr super-compile-time-table (static-class-info-compile-time-method-table (attribute super-static-info))
    #:attr super-field-count (static-class-info-field-count (attribute super-static-info)))
   (pattern
    (~seq)
    #:attr super #'#f
    #:attr super-method-table #f
    #:attr super-compile-time-table (make-immutable-free-id-table)
    #:attr super-field-count 0))

 (define (build-method-names+stxs super-compile-time-table method-mapping super-method-table)
   (define defined-methods (immutable-free-id-set (free-id-table-keys method-mapping)))
   (define all-methods
       (free-id-set-union defined-methods
                          (immutable-free-id-set (free-id-table-keys super-compile-time-table))))
   (define methods-count (free-id-set-count all-methods))
   (define method-names (make-vector methods-count))
   (define method-stx (make-vector methods-count))
   (for/fold ([new-method-idx (free-id-table-count super-compile-time-table)])
             ([method-name (in-free-id-set all-methods)])
     (cond
       [(dict-ref super-compile-time-table method-name #f)
        =>
        (λ (idx)
          (vector-set! method-names idx method-name)
          (if (free-id-set-member? defined-methods method-name)
              (vector-set! method-stx idx #`(method #,(dict-ref method-mapping method-name)))
              (vector-set! method-stx idx #`(vector-ref #,super-method-table #,idx)))
          new-method-idx)]
       [else
        (vector-set! method-names new-method-idx method-name)
        (vector-set! method-stx new-method-idx #`(method #,(dict-ref method-mapping method-name)))
        (add1 new-method-idx)]))
   (values (vector->list method-names)
           (vector->list method-stx)))

 ;; might also need the field names ...
 (struct static-class-info (super-class
                            compile-time-method-table
                            run-time-method-table-id
                            constructor-id
                            field-count))

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
    [(_ name super-class:extends-decl var:var-decl ... meth:meth-decl ...)
     (define run-time-method-table-id (generate-temporary 'runtime-method-table))
     (define super-method-table (attribute super-class.super-method-table))
     (define super-compile-time-table (attribute super-class.super-compile-time-table))
     (define super-field-count (attribute super-class.super-field-count))
     (define field-count (+ super-field-count (r:length (syntax->list #'(var.name ...)))))
     (define method-mapping
       (make-immutable-free-id-table
        (map cons (syntax->list #'(meth.name ...)) (syntax->list #'(meth ...)))))
     (define-values (method-names method-stxs)
       (build-method-names+stxs super-compile-time-table method-mapping super-method-table))
     (define/with-syntax super (attribute super-class.super))
     #`(begin
         (define #,run-time-method-table-id
           (let-syntax (#,@(for/list ([var-name (in-list (syntax->list #'(var.name ...)))]
                                      [i        (in-naturals (add1 super-field-count))]) ; 0 is method table, start after super-fields
                             #`[#,var-name
                                (make-set!-transformer
                                 (lambda (stx)
                                   (syntax-parse stx
                                     #:literals (set!)
                                     [(set! x val) #'(vector-set! this #,i val)]
                                     [x            #'(vector-ref this #,i)])))]))
             ;; method table
             (vector #,@method-stxs)))
         (define (constructor)
           (vector #,run-time-method-table-id
                   #,@(r:make-list field-count #f)))
         (define-syntax name
           (static-class-info
            super
            ;; compile-time method table
            (make-immutable-free-id-table
             (list
              #,@(for/list ([meth-name (in-list method-names)]
                            [i         (in-naturals)])
                   #`(cons #'#,meth-name #,i))))
            #'#,run-time-method-table-id
            #'constructor
            #,field-count)))]))

;; for post-processing
;; `define-method` (and `define-field`) is fixed syntax in `define-class`
(define-simple-macro (method meth:meth-decl)
  (lambda (receiver meth.arg-name ...)
    (syntax-parameterize
     ([this (make-rename-transformer #'receiver)])
     meth.body ...)))

(define-simple-macro (define-local x)
  (define x #f))

;; statements (rest are just re-exported from Racket, linguistic-reuse-style)
;; ~~~EXTRACT:refactor-if~~~
(define-syntax (if stx)
  (syntax-parse stx
    [(if test then else)
     (add-refactor-property
      (syntax/loc this-syntax (r:if test then else))
      (list (r:if (syntax-property stx 'mini-java) 'mini-java 'sexp-mini-java)
            (syntax-loc stx)
            (syntax-loc #'test)
            (syntax-loc #'then)
            (syntax-loc #'else)))]))
;; ~~~EXTRACT:refactor-if~~~

(define-syntax (while stx)
  (syntax-parse stx
    [(while test body ...)
     (define temp (generate-temporary 'loop))
     #`(let/ec local-break
         (syntax-parameterize ([break (syntax-parser [(break) #'(local-break)])])
           (let #,temp ()
             (when test
               body ...
               (#,temp)))))]))

;; expressions (except Racket re-exports)

(define-syntax (send stx)
  (syntax-parse stx
    [(_ the-class ; added by typechecking pass
        receiver method-name arg ...)
     #:do [(define ct-method-table
             (static-class-info-compile-time-method-table (syntax-local-value #'the-class)))]
     #`(let* ([receiver-val receiver]
              [rt-method-table (vector-ref receiver-val 0)]
              [method-index    #,(dict-ref ct-method-table #'method-name)]
              [method          (vector-ref rt-method-table method-index)])
         (method receiver-val arg ...))]))

;; ~~~EXTRACT:mj-new~~~
(define-syntax (new stx)
  (syntax-parse stx
    [(new the-class)
     #`(#,(static-class-info-constructor-id
           (syntax-local-value #'the-class)))]))
;; ~~~EXTRACT:mj-new~~~

(define-syntax 2dstate-machine
  (syntax-parser
    [t:2dstate-class
     (define mapping1 (attribute t.mapping))
     (define rows (attribute t.rows))
     (define columns (attribute t.columns))
     (define/with-syntax (transitions ...)
       (attribute t.method-names))
     (define/with-syntax (state ...)
       (attribute t.state-names))
     (define state-ints
       (for/hash ([s (in-syntax #'(state ...))]
                  [i (in-naturals)])
         (values (syntax-e s) i)))
     (define sorted-cells
       (filter
        (lambda (x) (not (or (equal? (caar x) 0)
                             (equal? (second (car x)) 0))))
        (sort
         (hash->list mapping1)
         #:key car
         (lambda (x y) (or (< (first x) (first y))
                           (< (second x) (second y)))))))
     (define/with-syntax (((_ new-state body ...) ...) ...)
       (group-by
        first
        (for/list ([x (in-list sorted-cells)])
         (define b (cdr x))
         (define-values (body state) (split-at b (- (length b) 1)))
         (list*
          (second (car x))
          (hash-ref state-ints (syntax-e (first state)))
          body))))
     (define/with-syntax name
       (attribute t.class-name))
     (define/with-syntax (state-int ...) (r:range 0 (length (syntax->list #'(state ...)))))
     (define cls
       #'(define-class name
           (define-field st)
           (define-method transitions ()
             (unless st (set! st 0))
             (case st
               [(state-int)
                body ...
                (set! st new-state)] ...)
             -42)
           ...))
     (syntax-property
      (syntax-property
       cls
       'disappeared-use
       (map
        syntax-local-introduce
        (syntax->list (attribute t.uses))))
      'disappeared-binding
      (map
       syntax-local-introduce
       (syntax->list #'(transitions ... state ...))))]))

