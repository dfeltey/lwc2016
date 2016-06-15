#lang racket

(provide build-context-table find-surrounding-context handle-expansion)

(require syntax/parse
         "property.rkt"
         "syntax-info.rkt")

(define-syntax-class val
  #:literal-sets (kernel-literals)
  (pattern (~or :identifier
                :boolean
                :str
                :char
                :keyword
                :number))
  (pattern (quote qv:quoted-val))
  (pattern (quote-syntax qv:quoted-val))
  (pattern (quote-syntax qv:quoted-val #:local))
  (pattern (#%plain-lambda formals expr ...))
  (pattern (case-lambda (formals expr ...) ...))
  (pattern (#%plain-app constructor:id v:val ...)
           #:when (syntax-property #'constructor 'constructor-for))
  ;; these are probably also values ...
  (pattern (#%top . v:id))
  (pattern (#%variable-reference v:id))
  (pattern (#%variable-reference (#%top . v:id)))
  (pattern (#%variable-reference)))

(define-syntax-class quoted-val
  (pattern v:val)
  (pattern ())
  (pattern (qv:quoted-val ...)))

(define value?
  (syntax-parser
    [:val #t]
    [_ #f]))

;; traverse fully expanded syntax and produce a hashtable
;; that maps (list source position span) elements to
;; (listof (list source position span)) or #f
;; a mapping to #f indicates the syntax object is not immediately
;; within an evaluation context so an if cannot be lifted
(define (build-context-table stx)
  (define parent-context-map (make-hash))
  (define refactor-info (make-hash))
  (define current-context-quoted? (make-parameter #f))
  
  (define (update-context-map this-loc [parent-loc #f])
    (define current (hash-ref parent-context-map this-loc (set)))
    (hash-set! parent-context-map
               this-loc
               (and parent-loc
                    current
                    (set-add current parent-loc))))
  (define (add-sub-exprs stxs p-loc)
    (for/fold ([E? #t])
              ([expr (in-syntax stxs)])
      (update-context-map (syntax-loc expr) (and E? p-loc))
      (loop expr)
      (value? expr)))
  
  (define (loop [stx stx])
    (hash-set! refactor-info
               (partial-syntax-loc stx)
               (cons (syntax-span stx) (get-refactor-property stx)))
    (define loc (and (not (current-context-quoted?)) (syntax-loc stx)))
    (syntax-parse stx
      #:literal-sets (kernel-literals)
      [(#%expression expr)
       (define expr-loc (syntax-loc #'expr))
       (update-context-map (syntax-loc #'expr) loc)
       (loop #'expr)]
      [(module name mod-path
         (#%plain-module-begin mod-level-form ...))
       (for ([mod-level-form (in-syntax #'(mod-level-form ...))])
         (update-context-map (syntax-loc mod-level-form))
         (loop mod-level-form))]
      [(module* name mod-path
         (#%plain-module-begin mod-level-form ...))
       (for ([mod-level-form (in-syntax #'(mod-level-form ...))])
         (update-context-map (syntax-loc mod-level-form))
         (loop mod-level-form))]
      [(#%provide raw-provide-spec ...) (void)]
      [(#%declare declaration-keyword ...) (void)]
      [(define-values (id ...) expr)
       (update-context-map (syntax-loc #'expr))
       (loop #'expr)]
      [(define-syntaxes (id ...) expr)
       (update-context-map (syntax-loc #'expr))
       (loop #'expr)]
      [(#%require raw-require-spec ...) (void)]
      [(#%plain-lambda formals expr ...)
       (for ([expr (in-syntax #'(expr ...))])
         (update-context-map (syntax-loc expr))
         (loop expr))]
      [(case-lambda (formals expr ...) ...)
       (for ([expr (in-syntax #'(expr ... ...))])
         (update-context-map (syntax-loc expr))
         (loop expr))]
      [(if test then else)
       (update-context-map (syntax-loc #'test) loc)
       (update-context-map (syntax-loc #'then))
       (update-context-map (syntax-loc #'else))
       (loop #'test)
       (loop #'then)
       (loop #'else)]   
      [(letrec-values ([(id ...) expr] ...) body ...)
       (for ([expr (in-syntax #'(expr ...))])
         (update-context-map (syntax-loc expr))
         (loop expr))
       (for ([body (in-syntax #'(body ...))])
         (update-context-map (syntax-loc body))
         (loop body))]
      [(set! id expr)
       (update-context-map (syntax-loc #'expr) loc)
       (loop #'expr)]
      [(quote datum)
       (update-context-map (syntax-loc #'datum))
       (parameterize ([current-context-quoted? #t])
         (loop #'datum))]
      [(quote-synax datum)
       (update-context-map (syntax-loc #'datum))
       (parameterize ([current-context-quoted? #t])
         (loop #'datum))]
      [(#%top . id) (void)]
      [(#%variable-reference id) (void)]
      [(#%variable-reference) (void)]
      ;; All of the following forms must be treated specially as at most one subexpression
      ;; within them is in an evaluation context
      [(let-values ([(id ...) expr] ...) body ...)
       (add-sub-exprs #'(expr ...) loc)
       (for ([body (in-syntax #'(body ...))])
         (update-context-map (syntax-loc body))
         (loop body))]
      [(begin top-level-form ...)
       (add-sub-exprs #'(top-level-form ...) loc)]
      [(begin-for-syntax top-level-form ...)
       (add-sub-exprs #'(top-level-form ...) loc)]
      [(begin0 expr ...)
       (add-sub-exprs #'(expr ...) loc)]
      [(with-continuation-mark key val res)
       (add-sub-exprs #'(key val res) loc)]
      [(#%plain-app expr ...)
       (add-sub-exprs #'(expr ...) loc)]
      [(any ...)
       (for ([any (in-syntax #'(any ...))])
         (update-context-map (syntax-loc any) loc))]
      [_ (void)]))
  (loop)
  (values
   (for/hash ([(k v) (in-hash parent-context-map)])
    (values k (and v (set->list v))))
   refactor-info))

;; takes a context mapping and a location and finds the largest
;; encompassing context
;; assumes that loc has the original source
(define (find-surrounding-context ctx-map loc)
  (printf "in find surrounding, loc is ~a\n" loc)
  (match-define (syntax-info source pos span) loc)
  (printf "match define here was ok ...\n")
  (let loop ([maybe-parent (hash-ref ctx-map loc #f)]
             [current loc])
    (define parent (and maybe-parent (set->list maybe-parent)))
    (cond
      [(and parent (= 1 (length parent)))
       (match-define (list (syntax-info p-source p-pos p-span)) parent)
       (if (contains (first  parent) loc)
           (loop (hash-ref ctx-map (first parent) #f) (first parent))
           (loop (hash-ref ctx-map (first parent) #f) current))]
      [parent
       ;(printf "ctx-map:\n~a\n" ctx-map)
       ;(error "TODO: what happens when it's in multiple contexts ...")
       #f]
      [else current])))

(define (contains p-loc loc)
  (match-define (syntax-info p-source p-pos p-span) p-loc)
  (match-define (syntax-info source pos span) loc)
  (and (equal? p-source source)
       (<= p-pos pos)
       (<= (+ pos span) (+ p-pos p-span))))



(define (handle-expansion stx path source cust)
  (printf "expansion handler called ...\n")
  (cond
    [(syntax? stx)
     (define-values (ctx-table rf-table)
       (build-context-table stx))
     (printf "returning non-false data ...\n")
     (list source ctx-table rf-table)]
    [else #f]))


  
(require (for-syntax syntax/parse))
(define-syntax (make-string stx)
  (syntax-parse stx
    [(_ body)
     #'(with-output-to-string
         (thunk (write 'body)))]))
(define-syntax-rule (:expand stx)
  (parameterize ([port-count-lines-enabled #t]
                 [current-namespace (make-base-namespace)])
    (expand (read-syntax (gensym) (open-input-string stx)))))