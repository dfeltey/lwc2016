#lang 2d racket
(require rackunit (for-syntax syntax/parse racket/syntax)
         racket/runtime-path)
(define-runtime-path here "state-machine-test.rkt")
(define-syntax (check-output stx)
  (syntax-parse stx
    [(_  stm ... (output ...))
     (define/with-syntax mod (gensym 'mod))
     #`(begin

         #,(datum->syntax stx
                          `(module ,#'mod "mini-java.rkt"
                             (extend "state-machine.rkt")
                             ,@(syntax->list #'(stm ...))))
         (check-equal?
          (with-output-to-string
           (thunk
              (dynamic-require `(submod ,here mod main) #f)))
          (~a #:separator "\n" output ... "")))]))

(check-output
 (class Main
   {
    public static void main (String [] args)
           {
            (System.out.println (((new Thing ()) do_the_thing ())))
            }
           

           }
   )
 (class Thing
   {
     (public int do_the_thing ()
                   {
                   (Machine m)
                   (m = (new Machine ()))
                   (System.out.println ((m same ())))
                   (System.out.println ((m switch ())))
                   (System.out.println ((m same ())))
                   (System.out.println ((m switch ())))
                   return 0
                   })
     })
 #2dstate-machine
 ╔══════════╦════════════════════════════════════╦════════════════════════════════════╗
 ║ Machine  ║                 a                  ║                  b                 ║
 ╠══════════╬════════════════════════════════════╬════════════════════════════════════╣
 ║ switch   ║  (System.out.println ("a to b")) b ║  (System.out.println ("b to a")) a ║
 ╠══════════╬════════════════════════════════════╬════════════════════════════════════╣
 ║ same     ║  (System.out.println ("a to a")) a ║  (System.out.println ("b to b")) b ║
 ╚══════════╩════════════════════════════════════╩════════════════════════════════════╝

 ("a to a"
  "0"
  "a to b"
  "0"
  "b to b"
  "0"
  "b to a"
  "0"
  "0")
 )
