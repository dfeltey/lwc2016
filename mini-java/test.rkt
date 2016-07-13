#lang s-exp "prefix-mini-java.rkt"

(define-class Fact
  (define-field x)
  (define-method fact (n)
    (define-local y)
    (= y 3)
    (= x 2)
    this
    (new Fact)
    ;; (send Fact this fact (+ x n y))
    (+ x n y)))

(main (System.out.println (+ 1 (send Fact (new Fact) fact 4))))
