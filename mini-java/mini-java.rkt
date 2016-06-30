#lang 2d racket

(require (for-syntax (except-in syntax/parse boolean)
                     racket/stxparam-exptime
                     "../editing/property.rkt"
                     "../editing/syntax-info.rkt")
         racket/stxparam
         (prefix-in r: racket))

(provide (all-defined-out)
         (rename-out [module-begin #%module-begin])
         (rename-out [require extend])
         #%datum)

(define-syntax-rule (define-literals (lit ...))
  (begin (define-syntax lit (syntax-rules ())) ...))
(define-literals (public static void
                         String int boolean
                         extends return if else while
                         System.out.println = new
                         length true false this))
(define (&& x y) (and x y))
(define (! x) (not x))
(define < r:<)
(define + r:+)
(define - r:-)
(define * r:*)

(begin-for-syntax
  
  ;; int []
  ;; boolean
  ;; int
  ;; <t:id>
  (define-splicing-syntax-class type
    #:literals (int boolean)
    (pattern (~seq int [])
             #:with id #'int)
    (pattern boolean
             #:with id #'boolean)
    (pattern int
             #:with id #'int)
    (pattern t:id
             #:with id #'t))
  
  ;; (<t:type> <name:id>)
  (define-syntax-class var-declaration
    (pattern (t:type name:id)
             #:with compiled
             (syntax-property
              #'(define name #f) ; syntax has no init value for vars
              'disappeared-use
              (list (syntax-local-introduce #'t.id)))))
  
  ;; (public <ret-t:type> <name:id> (<param-t:type> <param:id> ... ...) {<var:var-declaration> ... <body:statement> ... return <ret:expression>})
  (define-syntax-class method-declaration
    #:literals (public return)
    #:attributes (compiled name)
    (pattern (public ret-t:type name:id (param:param-group ...)
                     {var:var-declaration ...
                      body:statement ...
                      return ret:expression})
             #:with compiled
             (syntax-property
              #`(define-method (name param.name ...)
                  var.compiled ...
                  body.compiled ...
                  #,@(if #'ret
                         #'(ret.compiled)
                         #'()))
              'disappeared-use
              (map
               syntax-local-introduce
               (syntax->list #`(ret-t.id param.type ...))))))
  (define-splicing-syntax-class param-group
    #:attributes (name type)
    (pattern (~seq t:type name:id)
             #:with type #'t.id))
  
  ;; {<s:statement> ...}
  ;; (if (<tst:expression>) <thn:statement> else <els:statement>)
  ;; (while (<tst:expression>) <body:statement>)
  ;; (System.out.println (<arg:expression>))
  ;; (<lhs:id> = <rhs:expression>)
  ;; (<lhs:id> [<idx:expression>] = <rhs:expression>)
  (define-syntax-class statement
    #:literals (if else while System.out.println =)
    (pattern {s:statement ...}
             #:with compiled
             (if (null? (syntax->list #'(s ...)))
                 #'(r:void)
                 #`(begin s.compiled ...)))
    (pattern (if (tst:expression) thn:statement else els:statement)
             #:attr non-sexp? (syntax-property this-syntax 'mini-java)
             #:with compiled (add-refactor-property
                              (syntax/loc this-syntax (r:if tst.compiled thn.compiled els.compiled))
                              (list (if (attribute non-sexp?) 'mini-java'sexp-mini-java)
                                    (syntax-loc this-syntax)
                                    (syntax-loc #'tst)
                                    (syntax-loc #'thn)
                                    (syntax-loc #'els))))
    (pattern (while (tst:expression) body:statement)
             #:with compiled #`(let loop ()
                                 (when tst.compiled
                                   body.compiled
                                   (loop))))
    (pattern (System.out.println (arg:expression))
             #:with compiled #`(displayln arg.compiled))
    (pattern (lhs:id = rhs:expression)
             #:with compiled #`(set! lhs rhs.compiled))
    (pattern (lhd:id [idx:expression] = rhs:expression)
             #:with compiled #`(vector-set! lhs idx.compiled rhs.compiled)))
  
  ;; (<lhs:expression> <op:binop> <rhs:expression>)
  ;; (<array:expression> [<idx:expression>])
  ;; (<array:expression> length)
  ;; (new int [<len:expression>])
  ;; (new <c:identifier> ())
  ;; (super <meth:identifier> (<arg:expression> ...))
  ;; (<receiver:expression> <meth:identifier> (<arg:expression> ...))
  ;; true
  ;; false
  ;; this
  ;; (! <arg:expression>)
  ;; <var:identifier>
  ;; <n:exact-integer>
  ;; <str:str>
  (define-syntax-class expression
    #:literals (length true false new int ! this super)
    (pattern (lhs:expression op:binop rhs:expression)
             #:with compiled #`(op lhs.compiled rhs.compiled))
    (pattern (array:expression [idx:expression])
             #:with compiled #`(vector-ref array.compiled idx.compiled))
    (pattern (array:expression length)
             #:with compiled #`(vector-length array.compiled))
    (pattern (new int [len:expression])
             #:with compiled #`(make-vector len.compiled))
    (pattern (new the-class:id ())
             #:with compiled #`(r:new the-class))
    (pattern (super meth:id (arg:expression ...))
             #:with compiled #`(super meth arg.compiled ...))
    (pattern (receiver:expression meth:identifier (arg:expression ...))
             #:with compiled #`(send receiver.compiled meth arg.compiled ...))
    (pattern true
             #:with compiled #'#t)
    (pattern false
             #:with compiled #'#f)
    (pattern this
             #:with compiled #'r:this)
    (pattern (! arg:expression)
             #:with compiled #`(! arg.compiled))
    (pattern var:id
             #:with compiled #'var)
    (pattern n:exact-integer
             #:with compiled #'n)
    (pattern v:str
             #:with compiled #'v))
  
  (define-syntax-class binop
    #:literals (&& < + - *)
    (pattern &&)
    (pattern <)
    (pattern +)
    (pattern -)
    (pattern *))
  
  (define class->methods (make-hash))
  )

(define-syntax-parameter overrideable-methods '())
(define-syntax (super stx)
  (syntax-parse stx
    [(super method:id arg:expression ...)
     #:do [(define overrideable
             (syntax-parameter-value #'overrideable-methods))]
     #:fail-unless (and overrideable (member (syntax-e #'method) overrideable))
     "`super` can only be called inside overriding methods"
     #`(r:super method arg.compiled ...)]))
(define-syntax (define-method stx)
  (syntax-parse stx
    [(define-method (name:identifier param ...) body ...)
     #:do [(define overrideable
             (syntax-parameter-value #'overrideable-methods))]
     #`(#,(if (and overrideable (member (syntax-e #'name) overrideable))
              #'define/override
              #'define/public)
        (name param ...) body ...)]))

(define-syntax (module-begin stx)
  (syntax-parse stx
    [(_ body ...)
     #`(#%module-begin body ...)]))

;; (class <name:id> {public static void main (String [] <param:id>){<body:statement>}})
;; (class <name:id> {<var:var-declaration> ... <meth:method-declaration> ...})
;; (class <name:id> extends <parent:id> {<var:var-declaration> ... <meth:method-declaration> ...})
(define-syntax (class stx)
  ;; note: does not enforce the kind of brackets, just their presence
  (syntax-parse stx
    #:literals (public static void String extends)
    ;; main class
    [(class name:id {public static void main:id (String [] param:id)
                            {body:statement}})
     #:when (equal? (syntax-e #'main) 'main)
     ;; really just a function, which the body of the module should invoke
     (syntax-local-lift-module #`(module* main #f body.compiled))
     #'(r:void)]
    ;; regular class
    [(class name:id (~optional (~seq extends parent:id)
                               #:defaults ([parent #'object%]))
       {var:var-declaration ... meth:method-declaration ...})
     #:do [(hash-set! class->methods (syntax-e #'name) (syntax->datum #'(meth.name ...)))]
     #`(define name
         (syntax-parameterize
          ([overrideable-methods
            (hash-ref class->methods (syntax-e #'parent) #f)])
          (r:class parent
                   var.compiled ...
                   meth.compiled ...
                   (super-new))))]))


