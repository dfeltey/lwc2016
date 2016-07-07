#lang racket

;; typecheck, and turn the old, paren-but-infix syntax to the new, fully-prefix one,
;; with explicit types at `send`s

(require syntax/parse
         syntax/id-table
         racket/dict
         (for-template "prefix-mini-java.rkt"))

(provide typecheck-program)

(module literals racket
  (require "prefix-mini-java.rkt")
  (provide (all-defined-out))
  (define-literals (class public static void
                     String int boolean int-array
                     extends return else)))
(require (for-template (submod "." literals)))

(define-syntax-class var-declaration
  (pattern (type:id name:id)))

(define-syntax-class method-declaration
  #:literals (public return)
  (pattern (public return-type:id name:id (param:param-group ...)
                   {local:var-declaration ... body ... return ret})
    #:with (param-name ...) #'(param.name ...)
    #:with (param-type ...) #'(param.type ...)
    #:with (local-name ...) #'(local.name ...)
    #:with (local-type ...) #'(local.type ...)))
(define-splicing-syntax-class param-group
  (pattern (~seq type:id name:id)))

(define-syntax-class binop
  #:literals (&& < + - *)
  (pattern && #:with return-type #'boolean)
  (pattern <  #:with return-type #'boolean)
  (pattern +  #:with return-type #'int)
  (pattern -  #:with return-type #'int)
  (pattern *  #:with return-type #'int))

(define-syntax-class main-class
  #:literals (class public static void main)
  (pattern (class name:id {public static void main (String [] param:id) { body }})))

(define-syntax-class regular-class
  #:literals (class extends)
  (pattern (class name:id (~optional (~seq extends parent:id) ;; TODO actually implement inheritance and super
                                     #:defaults ([parent #'#f]))
             {var:var-declaration ... meth:method-declaration ...})
    #:with (field-name ...)         #'(var.name ...)
    #:with (field-type ...)         #'(var.type ...)
    #:with (method-name ...)        #'(meth.name ...)
    #:with (method-return-type ...) #'(meth.return-type ...)))

(define (empty-env) (make-immutable-free-id-table))
(define (extend-env env vars types)
  (for/fold ([env env])
      ([name (in-list (syntax->list vars))]
       [type (in-list (syntax->list types))])
    (dict-set env name type)))

;; a program is a list of class declarations (which includes main classes)
(define (typecheck-program stx)
  (define method-env (build-method-env (syntax->list stx)))
  #`(#,@(map (typecheck-class method-env) (syntax->list stx))))

;; maps (class-name . method-name) pairs to their return types
(define (build-method-env classes)
  (for/fold ([env (make-immutable-hash)])
      ([class (in-list classes)])
    (syntax-parse class
      [c:main-class
       env]
      [c:regular-class
       (for/fold ([env env])
           ([meth-name (in-list (syntax->list #'(c.method-name ...)))]
            [meth-type (in-list (syntax->list #'(c.method-return-type ...)))])
         (dict-set env (cons (syntax-e #'c.name) (syntax-e meth-name)) meth-type))])))

(define ((typecheck-class method-env) stx)
  (syntax-parse stx
    [c:main-class
     (quasisyntax/loc stx
       (main #,((typecheck-statement (empty-env) method-env #f) #'c.body)))]
    [c:regular-class
     #:do [(define env (extend-env (empty-env) #'(c.field-name ...) #'(c.field-type ...)))]
     (quasisyntax/loc stx
       (define-class c.name ;; TODO propagate extends as #:extends
         (define-field c.field-type c.field-name) ...
         #,@(map (typecheck-method env method-env #'c.name) (syntax->list #'(c.meth ...)))))]))

(define ((typecheck-method env method-env current-class) method)
  (syntax-parse method
    [meth:method-declaration
     #:do [(define body-env (extend-env (extend-env (empty-env)
                                                    #'(meth.param-name ...) #'(meth.param-type ...))
                                        #'(meth.local-name ...) #'(meth.local-type ...)))]
     (quasisyntax/loc method
       (define-method meth.return-type meth.name (meth.param ...)
         (define-local meth.local-type meth.local-name) ...
         #,@(map (typecheck-statement body-env method-env current-class)
                 (syntax->list #'(meth.body ...)))
         #,((typecheck-expression body-env method-env current-class) #'meth.ret)))]))

;; TODO could do actual typechecking. for now, just converts to prefix and recurs
(define ((typecheck-statement env method-env current-class) statement)
  (define t-s (typecheck-statement  env method-env current-class))
  (define t-e (typecheck-expression env method-env current-class))
  (syntax-parse statement
    #:literals (if else while System.out.println =)
    [(lhs:id = rhs)
     (quasisyntax/loc statement (= lhs #,(t-e #'rhs)))]
    [(lhs:id [idx] = rhs)
     (quasisyntax/loc statement (array= lhs #,(t-e #'idx) #,(t-e #'rhs)))]
    [(if (tst) thn else els)
     (quasisyntax/loc statement (if #,(t-e #'tst) #,(t-s #'thn) #,(t-s #'els)))]
    [(while (tst) body)
     (quasisyntax/loc statement (while #,(t-e #'tst) #,(t-s #'body)))]
    [(System.out.println (arg))
     (quasisyntax/loc statement (System.out.println #,(t-e #'arg)))]
    [{s ...}
     (quasisyntax/loc statement (compound #,@(map t-s (syntax->list #'(s ...)))))]))

(define type-key (gensym))
(define (add-type type stx)
  (syntax-property stx type-key type))
(define (get-type stx)
  (syntax-property stx type-key))

;; TODO as with typecheck-statement, could do actual checking. for now just makes type info explicit
(define ((typecheck-expression env method-env current-class) expression)
  (define t-e (typecheck-expression env method-env current-class))
  (syntax-parse expression
    #:literals (new int length true false ! this)
    [(new int [len])
     (add-type #'int-array (quasisyntax/loc expression
                             (new-int-array #,(t-e #'len))))]
    [(new the-class:id ())
     (add-type #'the-class (quasisyntax/loc expression
                             (new the-class)))]
    ;; TODO add `super` (and add as literal)
    [(lhs op:binop rhs)
     (add-type #'binop.return-type (quasisyntax/loc expression
                                     (op #,(t-e #'lhs) #,(t-e #'rhs))))]
    [(array [idx])
     (add-type #'int (quasisyntax/loc expression
                        (index #,(t-e #'array) #,(t-e #'idx))))]
    [(array length)
     (add-type #'int (quasisyntax/loc expression
                        (length #,(t-e #'array))))]
    [(-receiver meth:identifier (arg ...)) ; needs to be after the binop and new object cases
     (define receiver      (t-e #'-receiver))
     (define receiver-type (get-type receiver))
     (define return-type   (dict-ref method-env (cons (syntax-e receiver-type) (syntax-e #'meth))))
     (add-type return-type
               (quasisyntax/loc expression
                 (send #,receiver-type #,receiver meth
                       #,@(map t-e (syntax->list #'(arg ...))))))]
    [(! arg)
     (add-type #'boolean (quasisyntax/loc expression (! #,(t-e #'arg))))]
    [(~or true false)
     (add-type #'boolean expression)]
    [this
     (add-type current-class expression)]
    [var:id
     (add-type (dict-ref env #'var) expression)]
    [n:exact-integer
     (add-type #'int expression)]))
