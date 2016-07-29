#lang s-exp mini-java/prefix-mini-java

(define (main-method)
  (displayln
   (let* ([receiver-val (Runner:constructor)]
          [meth-table (vector-ref receiver-val 0)]
          [meth (vector-ref meth-table 0)])
     (meth receiver-val 10))))

(define Runner:runtime-method-table
  (vector
   (λ (this n)
     (define current #f)
     (vector-set! this 1 (Parity:constructor))
     (set! current 0)
     (letrec ([loop
               (λ ()
                 (when (< current n)
                   (if (let* ([receiver
                               (vector-ref this 1)]
                              [meth-table
                               (vector-ref
                                receiver
                                0)]
                              [meth
                               (vector-ref
                                meth-table
                                1)])
                         (meth receiver current))
                       (displayln current)
                       (void))
                   (set! current (+ current 1))
                   (loop)))])
       (loop))
     0)))

(define (Runner:constructor)
  (vector Runner:runtime-method-table #f))

(define-syntax Runner
  (static-class-info
   #f
   (make-immutable-free-id-table
    (list (cons #'run 0)))
   #'Runner:runtime-method-table
   #'Runner:constructor
   1))

(define Parity:runtime-method-table
  (vector
   (λ (this n)
     (and (! (== n 0))
          (let* ([meth-table (vector-ref this 0)]
                 [meth (vector-ref meth-table 1)])
            (meth this (- n 1)))))
   (λ (this n)
     (or (== n 0)
         (let* ([meth-table (vector-ref this 0)]
                [meth (vector-ref meth-table 0)])
           (meth this (- n 1)))))))

(define (Parity:constructor)
  (vector Parity:runtime-method-table))

(define-syntax Parity
  (static-class-info
   #f
   (make-immutable-free-id-table
    (list (cons #'is_odd 0) (cons #'is_even 1)))
   #'Parity:runtime-method-table
   #'Parity:constructor
   0))