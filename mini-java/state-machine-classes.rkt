#lang racket
(provide 2dstate-class cell-mapping)
(require syntax/parse racket/syntax)
(define-syntax-class 2dstate-class
  (pattern
   (form:id
    _ _
    cell:cell-mapping ...)
   #:attr (uses 1) #'(cell.uses ...)
   #:attr mapping
   (for*/hash ([o (in-syntax #'(cell ...))]
               [coord (in-syntax (first (syntax->list o)))])
     (syntax-parse coord
       [(a:nat b:nat)
        (values (list (syntax-e #'a) (syntax-e #'b))
                (syntax->list
                 (first
                  (rest (syntax->list o)))))]))
   #:attr rows (apply max (map second (hash-keys (attribute mapping))))
   #:attr columns (apply max (map first (hash-keys (attribute mapping))))
   #:attr (method-names 1)
   (for/list ([i (in-range 1 (add1 (attribute rows)))])
     (first
      (hash-ref (attribute mapping) (list 0 i))))
   #:attr (method-body 1) #'((cell.body ...) ...)
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
    (c:expr ... state:id))
   #:with (x ...) #'(a ...)
   #:with (y ...) #'(b ...)
   #:with uses #'state
   #:with (body ...) #'(c ...)))
