#lang s-exp mini-java/prefix-mini-java

(main
 (System.out.println
  (send Runner (new Runner) run 10)))




(define-class Runner
  (define-field check)
  (define-method run (n)
    (define-local current)
    (= check (new Parity))
    (= current 0)
    (while (< current n)
      (if
       (send Parity check is_even current)
       (compound
        (System.out.println current))
       (compound))
      (= current (+ current 1)))
    0))

























(define-class Parity
  (define-method is_odd (n)
    (&&
     (! (== n 0))
     (send Parity this is_even (- n 1))))
  (define-method is_even (n)
    (||
     (== n 0)
     (send Parity this is_odd (- n 1)))))














