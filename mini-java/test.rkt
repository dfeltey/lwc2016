#lang s-exp "prefix-mini-java.rkt"

(define-class Fact
  (define-field int x)
  (define-method int fact ([int n])
    (define-local int y)
    (= y 3)
    (= x 2)
    this
    (new Fact)
    ;; (send Fact this fact (+ x n y))
    (+ x n y)))

(main (System.out.println (+ 1 (send Fact (new Fact) fact 4))))
