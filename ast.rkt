#lang racket

;Macro to allow structure definition and provision
(define-syntax p-define-struct
  (syntax-rules ()
    [(_ (name inherit) fields)
     (begin
       (provide (struct-out name))
       (define-struct (name inherit) fields #:mutable #:prefab))]
    [(_ name fields)
     (begin
       (provide (struct-out name))
       (define-struct name fields #:mutable #:prefab))]))


;(make-src int int int int loc)
(p-define-struct src (line col pos span file))
(provide src->list)
(define (src->list src)
  (list (src-file src) (src-line src) (src-col src) (src-pos src) (src-span src)))

;;(make-id string src)
(p-define-struct id (string))

;;(make-class-def header (U #f MethodDeclaration) (U #f (list var-decl)) (U #f (list MethodDeclarations)))
;; members in file order
(p-define-struct class-def (header main fields methods))

;;(make-header id (list modifier) (list id) (list id))
(p-define-struct header (id modifiers extends))

;;(make-modifier symbol src)
(p-define-struct modifier (kind))

;;member = var-decl
;;       | method

  ;;(make-type-spec (U name type-var symbol) int src)
  ;; dim is for array dimensions
  (p-define-struct type-spec (name dim))

;;(make-var-decl id (list modifier) type-spec)
(p-define-struct var-decl (id modifiers type-spec))

;;(make-array-init (list (U expression array-init)) src)
(p-define-struct array-init (vals src))

;;(make-method (list modifier) type-spec id (list var-decl) Statement)
(p-define-struct method (modifiers type id parms body))

;;(make-initialize bool block src)
(p-define-struct initialize (static block src))

(provide statement?)

;statement? 'a -> bool
(define (statement? stmt)
  (or (ifS? stmt) (while? stmt) (block? stmt) (statement-expression? stmt)))

;statement => if
;           | throw
;           | return 
;           | while
;           | do
;           | for
;           | try
;           | switch
;           | block
;           | break
;           | continue
;           | label
;           | synchronized
;           | StatementExpression

;StatementExpression => call
;                     |  post-expr
;                     |  preExpr
;                     |  assignment
;                     |  class-alloc

;(make-ifS Expression Statement Statement)
(p-define-struct ifS (cond then else))

;(make-while Expression Statement)
(p-define-struct while (cond loop))

;(make-println Expression
(p-define-struct println (arg))

;(make-block (list VarDeclarations) (list Statement) (U #f Expression))
(p-define-struct block (var-decls stmts ret))

(provide statement-expression?)
;statement-expression?: StatementExpression -> bool
(define (statement-expression? stmt)
  (or (call? stmt)
      (unary? stmt)
      (assignment? stmt)
      (class-alloc? stmt)))

;(make-expr (U #f type) src)
(p-define-struct expr (types src)) 

;Expression => literal
;           |  bin-op
;           |  access
;           |  specified-this
;           |  call
;           |  class-alloc
;           |  array-alloc
;           |  cond-expression
;           |  array-access
;           |  post-expr
;           |  pre-expr
;           |  unary
;           |  cast
;           |  instanceof
;           |  assignment
;           |  check

;(make-literal type value)
(p-define-struct literal (type val))

;value => number

;(make-bin-op binary-op Expression Expression)
(p-define-struct bin-op (op left right))

;binary-op => && < + - * 

;(make-field-access Expression var-access)
(p-define-struct field-access (object field))

;(make-special-name string)
(p-define-struct special-name (name))

;(make-call Expression MethodName (list Expression))
(p-define-struct call (expr method-name args))

;MethodName => id

;(make-class-alloc id (list Expression))
(p-define-struct class-alloc (id args))

;(make-array-alloc type-spec  int)
(p-define-struct array-alloc (type dim))

;(make-array-access Expression Expression)
(p-define-struct array-access (name index))

;(make-unary UnaryOp Expression)
(p-define-struct unary (op expr))

;UnaryOp => !

;Note: lefthand side might be incorrect
;(make-assignment (U identifier array-access) symbol Expression)
(p-define-struct assignment (left op right))
