#lang racket

(require "if-refactor.rkt")

(let ([x #t])
  (+ 1 (if x 2 3)))

(+ (let-syntax ([x (λ (stx) #'#t)])
     (if x
         1
         2)) 1)

(define-syntax-rule (bad x)
  (append x 'x))

(bad (if #t null null))
(bad (+ 1 (if #t '(1) '(2))))
(bad (add1 (if #t '(3) '(4))))

(define-syntax-rule (weird y)
  (let ([x y])
    (+ 1 (if x 2 3))))

(weird #t)
(weird #f)

;; Can't lift outside of the empty let
;; because this can introduce unbound
;; variables from let-syntaxes
(let ()
  (if #t
      1
      2))

(define-syntax-rule (foo x) x)
(let ([x 3]) (+ (if (foo x) (+ x 3) 4) 1))

;; can't refactor because the if is duplicated
;; and one of the positions is not an E
(define-syntax-rule (double e)
  (+ e e))
(let ([x #t])
  (+
   (double (if x (begin (set! x #f) 1) 2))
   3))



(define-syntax-rule (L x)
  (λ () (+ x 1)))
(define-syntax-rule (B2 y)
  (begin (L y) (L y)))

(let ([z #t])
  (B2 (+ 4 (if z 2 3) 5)))

(define-syntax-rule (weird2 x)
  (begin (λ () (+ x 1)) (+ x 1)))
(let ([x #f])
  (weird2 (+ 2 (if x 1 2))))

(define-syntax-rule (weird3 x)
  (λ () (+ x 1)))
(let ([x #f])
  (weird3 (+ 2 (if x 1 2))))



















