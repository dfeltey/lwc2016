#lang racket
(require pict/code)
(provide (all-defined-out))


(define mj-simple-example
  (codeblock-pict
   #<<>>
#lang mini-java

class Even {          
 public boolean is_even(int n){
  return (n == 0) || this.is_odd(n - 1);
 }
 public boolean is_odd(int n) {
  return (n == 1) || this.is_even(n - 1);
 }
}
>>
   ))

(define mj-paren-example
  (codeblock-pict
   #<<>>
#lang s-exp mini-java/mini-java

(define-class Even
  (define-method boolean is_even ([int n])
    (or (== n 0) (send Even this is_odd (- n 1))))
  (define-method boolean is_odd ([int n])
    (or (== n 1) (send Even this is_even (- n 1)))))
>>
  ))

(define mj-compiled-vector
  (codeblock-pict
   #<<>>
#lang racket

(define Even:method-table
  (vector
   (λ (this n)
     (or (= n 0)
         ((vector-ref (vector-ref this 0) 1) this (- n 1))))
   (λ (this n)
     (or (= n 1)
         ((vector-ref (vector-ref this 0) 0) this (- n 1))))))
>>
   ))

(define mj-even-static-info
  (codeblock-pict
   #:keep-lang-line? #f
   #<<>>
#lang racket
(define-syntax Even
     (static-class-info
      Even:static-method-info
      #'Even:method-table
      #'Even:constructor))
>>
   ))


(define mj-statement-syntax-class
  (codeblock-pict
   #:keep-lang-line? #f
   #<<>>
#lang racket
(define-syntax-class statement
  #:literals (if else while System.out.println =)
  (pattern (if (tst:expression) thn:statement else els:statement)
           #:with compiled #'(r:if tst.compiled thn.compiled els.compiled))
  (pattern (System.out.println (arg:expression))
           #:with compiled #`(displayln arg.compiled))
  (pattern (lhs:id = rhs:expression)
           #:with compiled #`(set! lhs rhs.compiled))
  (pattern (lhd:id [idx:expression] = rhs:expression)
           #:with compiled #`(vector-set! lhs idx.compiled rhs.compiled))
  ...)
>>
  ))

(define 2d-state-machine
  (codeblock-pict
   #:keep-lang-line? #f
   #<<>>
#lang 2d s-exp mini-java/mini-java
#2dstate-machine
╔══════════╦══════════════════════════════════╦══════════════════════════════════╗
║ Receiver ║              wait_0              ║              wait_1              ║
╠══════════╬══════════════════════════════════╬══════════════════════════════════╣
║   zero   ║  (System.out.println ("ACK 0"))  ║  (System.out.println ("ACK 0"))  ║
║          ║              wait_1              ║              wait_1              ║
╠══════════╬══════════════════════════════════╬══════════════════════════════════╣
║   one    ║  (System.out.println ("ACK 1"))  ║  (System.out.println ("ACK 1"))  ║
║          ║              wait_0              ║              wait_0              ║
╚══════════╩══════════════════════════════════╩══════════════════════════════════╝
>>
   ))