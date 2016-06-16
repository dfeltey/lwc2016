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

