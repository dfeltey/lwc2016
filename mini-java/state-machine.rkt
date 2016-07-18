#lang 2d racket
(provide 2dstate-machine (all-from-out (submod "." stx-classes)))
(require (for-syntax syntax/parse racket/list racket/syntax racket/sequence) "prefix-mini-java.rkt")

(module stx-classes racket
  (provide 2dstate-class cell-mapping)
  (require syntax/parse racket/syntax)
  (define-syntax-class 2dstate-class
    ;#:literals (2dstate-machine)
    (pattern
     (2dstate-machine
      _ _
      cell:cell-mapping ...)
     #:attr (uses 1) #'(cell.uses ...)
     #:attr mapping
     (for*/hash ([o (in-syntax #'(cell ...))]
                 [coord (in-syntax (first (syntax->list o)))])
       (syntax-parse coord
         [(a:nat b:nat)
          (values (list (syntax-e #'a) (syntax-e #'b))
                  (rest (syntax->list o)))]))
     #:attr rows (apply max (map second (hash-keys (attribute mapping))))
     #:attr columns (apply max (map first (hash-keys (attribute mapping))))
     #:attr (method-names 1)
     (for/list ([i (in-range 1 (add1 (attribute rows)))])
       (first
        (hash-ref (attribute mapping) (list 0 i))))
     #:attr (method-body 1) #'(cell.body...)
     #:attr (state-names 1)
     (for/list ([i (in-range 1 (add1 (attribute columns)))])
       (first
        (hash-ref (attribute mapping) (list i 0))))
     #:attr class-name
     (first
      (hash-ref (attribute mapping) '(0 0)))))
  (define-syntax-class cell-mapping
    (pattern
     (((a:nat b:nat) ...)
      c:expr ... state:id)
     #:with (x ...) #'(a ...)
     #:with (y ...) #'(b ...)
     #:with uses #'state
     #:with (body ...) #'(c ...))))

(require (submod "." stx-classes)
         (for-syntax (submod "." stx-classes)))

(define-syntax 2dstate-machine
  (syntax-parser
    [t:2dstate-class
     (define mapping
       (attribute t.mapping))
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
         (hash->list mapping)
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
     (define/with-syntax (state-int ...) (range 0 (length (syntax->list #'(state ...)))))
     (define cls
       #'(define-class name
           (define-field st)
           (define-method transition ()
             (unless st (= st 0))
             (case st
               [(state-int)
                body ...
                (set! st new-state)] ...))
           ...)
       #;
       #`(class name
           {
            (int st)
            #,@(for/list
                   ([transition (in-syntax #'(transitions ...))]
                    [body (in-syntax #'((body ...) ...))]
                    [new-state (in-syntax #'((new-state ...) ...))])
                 #`(public int #,transition ()
                           {
                            (if ((! st))
                                {(st = 0)}
                                else
                                {})
                            #,(for/fold ([stx #'{}])
                                        ([state-int (in-naturals)]
                                         [body (in-syntax body)]
                                         [new-state (in-syntax new-state)])
                                #`(if (((st < (#,state-int + 1)) && ((#,state-int - 1) < st)))
                                      {#,body
                                       (st = #,new-state)}
                                      else
                                      #,stx))
                            return 0
                            }))
            }))
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
