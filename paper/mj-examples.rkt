#lang racket
(require pict pict/code)
(provide (all-defined-out))

(require (for-syntax syntax/parse))

(define mj-simple-example
  (codeblock-pict
   (port->string (open-input-file "../mini-java/even-odd.rkt"))))

(define parenthesized-mj-example
  (codeblock-pict
   (port->string (open-input-file "../mini-java/even-odd-prefix.rkt"))))

(define (extract file [name ""] #:lang [lang "racket"] #:prefix-lang? [prefix? #t])
  (define marker (~a ";; ~~~EXTRACT:" name "~~~"))
  (define-values (_1 _2 lines)
    (for/fold ([done? #f]
               [keep? #f]
               [lines null])
              ([line (in-lines (open-input-file file))]
               #:break done?)
      (cond
        [(equal? marker line) (values keep? (not keep?) lines)]
        [keep? (values done? keep? (cons line lines))]
        [else (values done? keep? lines)])))
  (string-join (reverse lines)
               "\n"
               #:before-first (or (and prefix? (string-append "#lang " lang "\n"))
                                  "")))

(define-syntax (define-extract stx)
  (syntax-parse stx 
    [(_ name file (~or (~optional (~seq #:lang lang))
                       (~optional (~seq #:keep-lang-line? keep?))) ...)
     #`(define name
         (codeblock-pict
          #:keep-lang-line? #,(or (attribute keep?) #'#t)
          (extract file 'name #,@(if (attribute lang) #'(#:lang lang) #'()))))]))

(define-extract mj-new "../mini-java/prefix-mini-java.rkt" #:keep-lang-line? #f)
(define-extract typecheck-mod-beg "../mini-java/infix-mini-java.rkt" #:keep-lang-line? #f)
(define-extract add-tool-tips "../mini-java/typecheck.rkt" #:keep-lang-line? #f)

(define refactor-impl
  (codeblock-pict
   (string-append
    (extract "../editing/property.rkt" 'refactor-prop)
    "\n"
    (extract "../mini-java/prefix-mini-java.rkt" 'refactor-if #:prefix-lang? #f))))

(define break-impl
  (codeblock-pict
   (string-append
    (extract "../mini-java/prefix-mini-java.rkt" 'break-param)
    "\n\n"
    (extract "../mini-java/prefix-mini-java.rkt" 'while+break #:prefix-lang? #f))))

(define mj-while-macro
  (codeblock-pict #:keep-lang-line? #f
   #<<>>
#lang racket
(define-syntax (while stx)
  (syntax-parse stx
    [(while test body ...)
     #`(letrec ([loop (λ () (when test body ... (loop)))])
         (loop))]))
>>
  ))

(define mj-parity-compiled
  (codeblock-pict
   #<<>>
#lang racket

(define Parity:method-table
  (vector
   (λ (this n)
     (or (== n 0)
         ((vector-ref (vector-ref this 0) 1) this (- n 1))))
   (λ (this n)
     (and (! (== n 0))
          ((vector-ref (vector-ref this 0) 0) this (- n 1))))))

(define (Parity:constructor)
  (vector Parity:method-table))

(define-syntax Parity
  (static-class-info
   Parity:static-method-info
   #'Parity:method-table
   #'Parity:constructor
   0))
>>
   ))

(define mj-send-macro
  (codeblock-pict
   #:keep-lang-line? #f
   #<<>>
#lang racket
(define-syntax (send stx)
  (syntax-parse stx
    [(send the-class
        receiver method-name arg ...)
     (define ct-method-table
       (static-class-info-compile-time-method-table
        (syntax-local-value #'the-class)))
     #`(let* ([receiver-val receiver]
              [rt-method-table (vector-ref receiver-val 0)]
              [method-index    #,(dict-ref ct-method-table #'method-name)]
              [method          (vector-ref rt-method-table method-index)])
         (method receiver-val arg ...))]))
>>
  ))

(define 2d-state-machine-text
  #<<>>
#lang mini-java
class StateMachineRunner {
    public int doTheThing() {
        Receiver r;
        r = new Receiver();
        System.out.println(r.one());
        System.out.println(r.zero());
        System.out.println(r.zero());
        System.out.println(r.one());
        return 0;
    }
}

#2dstate-machine
╔══════════╦══════════════════════════════════╦══════════════════════════════════╗
║ Receiver ║              wait_0              ║              wait_1              ║
╠══════════╬══════════════════════════════════╬══════════════════════════════════╣
║   zero   ║       System.out.println(0);     ║       System.out.println(1);     ║
║          ║              wait_1              ║              wait_1              ║
╠══════════╬══════════════════════════════════╬══════════════════════════════════╣
║   one    ║       System.out.println(2);     ║       System.out.println(3);     ║
║          ║              wait_0              ║              wait_0              ║
╚══════════╩══════════════════════════════════╩══════════════════════════════════╝
>>
  )

(define 2d-state-machine
  (codeblock-pict
   #:keep-lang-line? #f
   2d-state-machine-text))

(define 2d-state-exped
  (code
   (define-class Receiver
     (define-field state)
     (define-method zero ()
       (unless state (set! state 0))
       (case state
         [(0)
          (System.out.println 0)
          (set! state 1)]
         [(1)
          (System.out.println 1)
          (set! state 1)]))
     (define-method one ()
       (unless state (set! state 0))
       (case state
         [(0)
          (System.out.println 2)
          (set! state 0)]
         [(1)
          (System.out.println 3)
          (set! state 0)])))))



;; MiniJava implementation phase diagram
(define (make-lang-box txt [w 165] [h 20])
    (cc-superimpose
     (text txt)
     (rounded-rectangle w h)))
(define MJ
  (make-lang-box "MiniJava"))
(define AST
  (make-lang-box "Abstract Syntax Tree"))
(define PMJ
  (make-lang-box "Parenthesized MiniJava"))
(define RKT
  (make-lang-box "Racket"))
(define EXP
  (make-lang-box "Fully-Expanded Code"))
(define lang-phases (list MJ AST PMJ RKT EXP))
(define combined (apply vc-append 30 lang-phases))
(define pairs
  (reverse
   (let loop ([pairs null]
              [lst lang-phases])
     (cond
       [(= 2 (length lst))
        (cons (apply cons lst) pairs)]
       [else
        (match-define (list f s r ...) lst)
        (loop (cons (cons f s) pairs)
              (rest lst))]))))
(define explanations
  (list "Lexing + Parsing"
        "Type Checking"
        "Macro Expansion"
        "More Macro Expansion"))

(define the-x 20)
(define (find-bot pict find)
  (define-values (x y) (cb-find pict find))
  (values the-x y))
(define (find-top pict find)
  (define-values (x y) (ct-find pict find))
  (values the-x y))

(define pipeline-diagram
  (for/fold ([diagram combined])
            ([pair (in-list pairs)]
             [explanation (in-list explanations)]
             [i (in-naturals 1)])
    (define count (text (~a (number->string i) ".   ")))
    (define count-width (pict-width count))
    (match-define (cons top bot) pair)
    (define txt (text explanation))
    (define adjustment (/ (- (pict-width txt)count-width) 2))
    (pin-arrow-line 5 diagram
                    top find-bot
                    bot find-top
                    #:line-width 1
                    #:label (hc-append count txt)
                    #:x-adjust-label adjustment)))
(define pipeline+mj-example
  (hc-append  20 pipeline-diagram
             mj-simple-example))
(define refactoring-pict
  (scale
   (hc-append
    (bitmap "refactoring.png")
    (bitmap "after-refactor.png"))
   .6))
