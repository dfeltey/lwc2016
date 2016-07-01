#lang 2d racket
(provide 2dstate-machine)
(require (for-syntax syntax/parse racket/list racket/syntax racket/sequence) "mini-java.rkt")

(begin-for-syntax
  (define-syntax-class cell-mapping
    (pattern
     (((a:nat b:nat))
      c:expr ... state:id)
     #:with uses #'state)))

(define-syntax 2dstate-machine
  (syntax-parser
    [(_ _ _ obj:cell-mapping ...)
     (define mapping
       (for*/hash ([o (in-syntax #'(obj ...))]
                   [coord (in-syntax (first (syntax->list o)))])
         (syntax-parse coord
           [(a:nat b:nat)
            (values (list (syntax-e #'a) (syntax-e #'b))
                    (rest (syntax->list o)))])))
     (define rows (apply max (map second (hash-keys mapping))))
     (define columns (apply max (map first (hash-keys mapping))))
     (define/with-syntax (transitions ...)
       (for/list ([i (in-range 1 (add1 rows))])
         (first
          (hash-ref mapping (list 0 i)))))
     (define/with-syntax (state ...)
       (for/list ([i (in-range 1 (add1 columns))])
         (first
          (hash-ref mapping (list i 0)))))
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
     (define/with-syntax (((_ new-state . body) ...) ...)
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
       (first
        (hash-ref mapping '(0 0))))

     (define cls
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
        (syntax->list #'(obj.uses ...))))
      'disappeared-binding
      (map
       syntax-local-introduce
       (syntax->list #'(transitions ... state ...))))]))
