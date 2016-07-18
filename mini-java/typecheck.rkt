#lang racket

;; typecheck, and turn the old, paren-but-infix syntax to the new, fully-prefix one,
;; with explicit types at `send`s

(require syntax/parse
         syntax/id-table
         syntax/id-set
         racket/dict
         unstable/error
         racket/syntax
         (for-template "prefix-mini-java.rkt")
         "state-machine-classes.rkt")

(provide typecheck-program)

(module literals racket
  (require "prefix-mini-java.rkt")
  (provide (all-defined-out))
  (define-literals (class public static void
                     String int boolean int-array
                     extends return else)))
(require (for-template (submod "." literals)))

;; Env
(define current-env (make-parameter (make-immutable-free-id-table)))

;; Types
(struct type ())
(struct top-type type ())
(struct class-type type (name super methods) #:transparent)
(struct object-type type (class-type) #:transparent)
(struct method-type type (name arg-types res-type) #:transparent)
(struct base-type type (key) #:transparent)
(define int-type (base-type (gensym)))
(define bool-type (base-type (gensym)))
(define int-array-type (base-type (gensym)))
(struct binop-type (left right res) #:transparent)

(define (make-class-type name super methods)
  (class-type
   name
   super
   (make-immutable-free-id-table
    (for/list ([method (in-list methods)])
      (match-define (method-type name _ _) method)
      (cons name method)))))

;; (or/c type? identifier?) -> type?
(define (resolve-type t [env (current-env)])
  (define (error)
    (raise-syntax-error 'resolve-type (~a "No type for identifier: " (syntax-e t)) t))
  (cond
    [(type? t) t]
    [(identifier? t) (free-id-table-ref env t error)]
    [else (raise-syntax-error 'resolve-type "This shouldn't happen!")]))

(define (resolve-object-type t [env (current-env)])
  (define rt (resolve-type t env))
  (cond
    [(class-type? rt) (object-type rt)]
    [else rt]))

(define (subtype? t1 t2 [env (current-env)])
  (define rty1 (resolve-type t1 env))
  (define rty2 (resolve-type t2 env))
  (match* (rty1 rty2)
    [(_ (top-type)) #t]
    [((base-type k1) (base-type k2)) (eq? k1 k2)]
    [((class-type _ _ _) (class-type n2 _ _))
     (free-id-set-member? (ancestors rty1 env) n2)]
    [((method-type n1 arg-tys1 res-ty1) (method-type n2 arg-tys2 res-ty2))
     (and (= (length arg-tys1) (length arg-tys2))
          (subtype? res-ty1 res-ty2 env)
          (for/and ([t1 (in-list arg-tys1)]
                    [t2 (in-list arg-tys2)])
            (subtype? t2 t1 env)))]
    [((object-type cty1) (object-type cty2))
     (subtype? cty1 cty2 env)]
    [(_ _) #f]))

;; Returns a free-id-set of all ancestors
;; of the given class-type
(define (ancestors class-ty [env (current-env)])
  (let loop ([ancestors (immutable-free-id-set)]
             [current class-ty])
    (match (and current (resolve-type current env))
      [(class-type n s _)
       (loop (free-id-set-add ancestors n)
             s)]
      [_ ancestors])))

(define (find-method-type meth-name class-id [env (current-env)])
  (cond
    [class-id
     (define resolved-super (resolve-type class-id env))
     (match resolved-super
       [(class-type super-name super-super super-method-dict)
        (define method-type (dict-ref super-method-dict meth-name #f))
        (or method-type
            (find-method-type meth-name super-super env))]
       [_ (raise-syntax-error 'find-inherited-method-type
                              "Super type was not a class type")])]
    [else #f]))
     

(define-splicing-syntax-class type-sc
  #:literals (int boolean)
  (pattern (~seq int ())
           #:attr type int-array-type)
  (pattern int
           #:attr type int-type)
  (pattern boolean
           #:attr type bool-type)
  (pattern class-name:id
           #:attr type #'class-name))

(define-syntax-class var-declaration
  (pattern (ty:type-sc name:id)
           #:attr type (attribute ty.type)))

(define-syntax-class method-declaration
  #:literals (public return)
  (pattern (public return-type:type-sc name:id (param:param-group ...)
                   {local:var-declaration ... body ... return ret})
    #:with (param-name ...) #'(param.name ...)
    #:with (local-name ...) #'(local.name ...)
    #:attr local-names (attribute local.name)
    #:attr local-types (attribute local.type)
    #:attr param-names (attribute param.name)
    #:attr param-types (attribute param.type)
    #:attr method-type (method-type #'name (attribute param.type) (attribute return-type.type))
    #:attr extend-names (append (attribute param-names) (attribute local-names))
    #:attr extend-types (append (attribute param-types) (attribute local-types))))
    
(define-splicing-syntax-class param-group
  (pattern (~seq ty:type-sc name:id)
           #:attr type (attribute ty.type)))

(define-syntax-class binop
  #:literals (&& < + - * || ==)
  (pattern ==
           #:with return-type #'boolean
           #:attr type (binop-type (top-type) (top-type) bool-type))
  (pattern &&
           #:with return-type #'boolean
           #:attr type (binop-type bool-type bool-type bool-type))
  (pattern ||
           #:with return-type #'boolean
           #:attr type (binop-type bool-type bool-type bool-type))
  (pattern <
           #:with return-type #'boolean
           #:attr type (binop-type int-type int-type bool-type))
  (pattern +
           #:with return-type #'int
           #:attr type (binop-type int-type int-type int-type))
  (pattern -
           #:with return-type #'int
           #:attr type (binop-type int-type int-type int-type))
  (pattern *
           #:with return-type #'int
           #:attr type (binop-type int-type int-type int-type)))

(define-syntax-class main-class
  #:literals (class public static void main)
  (pattern (class name:id {public static void main (String [] param:id) { body }})
    #:attr class-type (make-class-type #'name #f null)))

(define-syntax-class regular-class
  #:literals (class extends)
  (pattern (class name:id (~optional (~seq extends parent:id) ;; TODO actually implement inheritance and super
                                     #:defaults ([parent #f]))
             {var:var-declaration ... meth:method-declaration ...})
    #:with (field-name ...)         #'(var.name ...)
    #:with (method-name ...)        #'(meth.name ...)
    #:with (method-return-type ...) #'(meth.return-type ...)
    #:attr field-names (attribute var.name)
    #:attr field-types (attribute var.type)
    #:attr extends-stx (if (attribute parent) #'(#:extends parent) #'())
    #:attr class-type (make-class-type #'name (attribute parent) (attribute meth.method-type))))

(define (empty-env) (make-immutable-free-id-table))
(define (extend-env env vars types)
  (for/fold ([env env])
      ([name (in-list vars)]
       [type (in-list types)])
    (dict-set env name (resolve-object-type type))))

(define-syntax-rule (with-extended-env vars types . body)
  (parameterize ([current-env (extend-env (current-env) vars types)]) . body))

;; a program is a list of class declarations (which includes main classes)
(define (typecheck-program stx)
  (define toplevel-env (build-toplevel-env (syntax->list stx)))
  #`(#,@(parameterize ([current-env toplevel-env])
          (for/list ([class (in-syntax stx)])
            (typecheck-class class)))))

;; maps (class-name . method-name) pairs to their return types
(define (build-toplevel-env classes)
  (for/fold ([env (make-immutable-free-id-table)])
      ([class (in-list classes)])
    (syntax-parse class
      [c:main-class
       (dict-set env #'c.name (attribute c.class-type))]
      [c:regular-class
       (dict-set env #'c.name (attribute c.class-type))]
      [c:2dstate-class
       (dict-set env
                 #'c.class-name
                 (make-class-type
                  #'c.class-name
                  #f
                  (for/list ([n (in-syntax (attribute c.method-names))])
                    (method-type n null int-type))))])))

(define (typecheck-class stx [toplevel-env (current-env)])
  (syntax-parse stx
    [c:main-class
     (quasisyntax/loc stx
       (main #,(typecheck-statement #'c.name #'c.body toplevel-env)))]
    [c:regular-class
     (define extends-clause (attribute c.extends-stx))
     (quasisyntax/loc stx
       (define-class c.name #,@extends-clause
         (define-field c.field-name) ...
         #,@(with-extended-env (attribute c.field-names) (attribute c.field-types)
              (for/list ([method (in-syntax #'(c.meth ...))])
                (typecheck-method #'c.name method)))))]
    [(~and
      class:2dstate-class
      (form
       co1 co2
       cell:cell-mapping ...))
     (define/with-syntax ((new-body ...) ...)
       (for/list ([b* (in-syntax #'((cell.body ...) ...))])
         (for/list ([stmt (in-syntax b*)])
           (typecheck-statement (attribute class.class-name) stmt))))
     (quasisyntax/loc stx
       (form
        co1 co2
        (((cell.x cell.y) ...)
         (new-body ... cell.uses))
        ...))]))

;; FIXME: typecheck-expression returns 2 values
;; need to check the actual return type against the expected one
;; figure out the places where identifiers are passed through as types
;; so that I can resolve them when necessary
;; Also all field declarations and local variable types need to be checked that
;; they actually exist ...
(define (typecheck-method current-class-name method [env (current-env)])
  (syntax-parse method
    [meth:method-declaration
     (define current-class-type (resolve-type current-class-name))
     ;; This might need better error handling ...
     (match-define (class-type name super method-dict) current-class-type)
     (define inherited-method-type (find-method-type #'meth.name super env))
     (define method-ty (attribute meth.method-type))
     (unless (or (not inherited-method-type)
                 (subtype? method-ty inherited-method-type env))
       (raise-syntax-error* "Method type not a subtype of inherited method"
                            #'meth.name
                            "in class" (syntax-e current-class-name)))
     (define expected-result-type
       (resolve-object-type (method-type-res-type method-ty)))
     
     (with-extended-env (attribute meth.extend-names) (attribute meth.extend-types)
       (define-values (ret-ty ret-stx) (typecheck-expression current-class-name #'meth.ret))
       (unless (subtype? ret-ty expected-result-type)
         (raise-syntax-error* "return type mismatch"
                              method
                              #f))
       (quasisyntax/loc method
         (define-method meth.name (meth.param-name ...)
           (define-local meth.local-name) ...
           #,@(for/list ([statement (in-syntax #'(meth.body ...))])
                (typecheck-statement current-class-name statement))
           #,ret-stx)))]))

;; TODO could do actual typechecking. for now, just converts to prefix and recurs
(define (typecheck-statement current-class-name statement [env (current-env)])
  (define (t-s s) (typecheck-statement current-class-name s env))
  (define (t-e e) (typecheck-expression current-class-name e env))
  (syntax-parse statement
    #:literals (if else while System.out.println = break)
    [(break) (quasisyntax/loc statement (break))]
    [(lhs:id = rhs)
     (define lhs-ty (resolve-type #'lhs))
     (define-values (rhs-ty rhs-stx) (t-e #'rhs))
     (unless (subtype? rhs-ty lhs-ty)
       (raise-syntax-error* "Type mismatch in assignment"
                            statement                            
                            #f))
     (quasisyntax/loc statement (= lhs #,rhs-stx))]
    [(lhs:id [idx] = rhs)
     (define lhs-ty (resolve-type #'lhs))
     (define-values (idx-ty idx-stx) (t-e #'ids))
     (define-values (rhs-ty rhs-stx) (t-e #'rhs))
     (unless (subtype? lhs-ty int-array-type)
       (raise-syntax-error* "Type mismatch in assignment"
                            #'=
                            #f))
     (unless (subtype? idx-ty int-type)
       (raise-syntax-error* "Type mismatch in array index"
                            #'=
                            #f))
     (unless (subtype? rhs-ty int-type)
       (raise-syntax-error* "Type mismatch in array assignment"
                            #'=
                            #f))
     (quasisyntax/loc statement (array= lhs #,idx-stx #,rhs-stx))]
    [(if (tst) thn else els)
     (define-values (tst-ty tst-stx) (t-e #'tst))
     (unless (subtype? tst-ty bool-type)
       (raise-syntax-error* "Type mismatch in if condition"
                            #'if
                            #f))
     (quasisyntax/loc statement (if #,tst-stx #,(t-s #'thn) #,(t-s #'els)))]
    [(while (tst) body)
     (define-values (tst-ty tst-stx) (t-e #'tst))
     (unless (subtype? tst-ty bool-type)
       (raise-syntax-error* "Type mismatch in while condition"
                            #'while
                            #f))
     (quasisyntax/loc statement (while #,tst-stx #,(t-s #'body)))]
    [(System.out.println (arg))
     (define-values (arg-ty arg-stx) (t-e #'arg))
     (unless (subtype? arg-ty int-type)
       (raise-syntax-error* "Type mismatch"
                            #'System.out.println
                            #f))
     (quasisyntax/loc statement (System.out.println #,arg-stx))]
    [{s ...}
     (quasisyntax/loc statement (compound #,@(map t-s (syntax->list #'(s ...)))))]))

(define type-key (gensym))
(define (add-type type stx)
  (syntax-property stx type-key type))
(define (get-type stx)
  (syntax-property stx type-key))

;; TODO as with typecheck-statement, could do actual checking. for now just makes type info explicit
(define (typecheck-expression current-class expression [env (current-env)])
  (define (t-e expr) (typecheck-expression current-class expr env))
  (syntax-parse expression
    #:literals (new int length true false ! this)
    [(new int [len])
     (define-values (len-ty len-stx) (t-e #'len))
     (unless (subtype? len-ty int-type env)
       (raise-syntax-error* "Type mismatch"
                            #'len
                            #f
                            "expected" "int"))
     (values int-array-type
             (quasisyntax/loc expression
               (new-int-array #,len-stx)))]
    [(new the-class:id ())
     (define class-ty (resolve-type #'the-class env))
     (match class-ty
       [(class-type n s ms)
        (values (object-type class-ty)
                (quasisyntax/loc expression (new the-class)))]
       [_ (raise-syntax-error* "Type mismatch"
                               #'the-class
                               #f
                               "expected" "a class name")])]
    ;; TODO add `super` (and add as literal)
    [(lhs op:binop rhs)
     (match-define (binop-type left-ty right-ty res-ty) (attribute op.type))
     (define-values (lhs-ty lhs-stx) (t-e #'lhs))
     (define-values (rhs-ty rhs-stx) (t-e #'rhs))
     (unless (and (subtype? lhs-ty left-ty env)
                  (subtype? rhs-ty right-ty env))
       (raise-syntax-error* "Type mismatch"
                            #'op
                            #f))
     (values res-ty
             (quasisyntax/loc expression
               (op #,lhs-stx #,rhs-stx)))]
    [(array [idx])
     (define-values (array-ty array-stx) (t-e #'array))
     (define-values (idx-ty idx-stx) (t-e #'idx))
     (unless (and (subtype? idx-ty int-type env)
                  (subtype? array-ty int-array-type env))
       (raise-syntax-error* "Type mismatch"
                            expression
                            #f))
     (values int-type
             (quasisyntax/loc expression
               (index #,array-stx #,idx-stx)))]
    [(array length)
     (define-values (array-ty array-stx) (t-e #'array))
     (unless (subtype? array-ty int-array-type env)
       (raise-syntax-error* "Type mismatch"
                            expression
                            #f))
     (values int-type
             (quasisyntax/loc expression
               (vector-length #,array-stx)))]
    [(-receiver meth:identifier (arg ...)) ; needs to be after the binop and new object cases
     (define-values (recvr-ty recvr-stx) (t-e #'-receiver))
     (match recvr-ty
       [(object-type class-ty)
        (match-define (class-type class-name _ _) class-ty)
        (define method-ty (find-method-type #'meth class-ty env))
        (unless method-ty
          (raise-syntax-error* "Method not found"
                               #'meth
                               #f))
        (match-define (method-type mname marg-types mres-type) method-ty)
        (define-values (garg-types garg-stxs)
          (for/lists (garg-types garg-stxs)
                     ([garg (in-syntax #'(arg ...))])
            (t-e garg)))
        (unless (= (length garg-types) (length marg-types))
          (raise-syntax-error* "Wrong number of arguments to method"
                               #'meth
                               #f))
        (unless (for/and ([garg-ty (in-list garg-types)]
                          [marg-ty (in-list marg-types)])
                  (subtype? garg-ty marg-ty env))
          (raise-syntax-error* "Type mismatch"
                               expression
                               #f))
        (values (resolve-object-type mres-type)
                (quasisyntax/loc expression
                  (send #,class-name #,recvr-stx meth #,@garg-stxs)))]
       [_
        (raise-syntax-error* "Type mismatch"
                             expression
                             #f
                             "expected" "object type")])]
    [(! arg)
     (define-values (arg-ty arg-stx) (t-e #'arg))
     (unless (subtype? arg-ty bool-type env)
       (raise-syntax-error* "Type mismatch"
                            #'arg
                            #f
                            "expected" "boolean"))
     (values bool-type
             (quasisyntax/loc expression (! #,arg-stx)))]
    [(~or true false)
     (values bool-type expression)]
    [this
     (define class-ty (resolve-type current-class env))
     (values (object-type class-ty) expression)]
    [var:id
     (define var-ty (resolve-type #'var env))
     (unless var-ty
       (raise-syntax-error* "No type for identifier"
                            #'var
                            #f))
     (values var-ty expression)]
    [n:exact-integer
     (values int-type expression)]))
