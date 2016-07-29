#lang racket
(require pict pict/code "setup.rkt"
         racket/runtime-path)
(provide (all-defined-out))

(require (for-syntax syntax/parse))

(define (program->figure* paths #:first-line first-line #:last-line last-line
                          #:buffer [buffer 10])
  ;; this function uses 'codeblock-pict' on each line individually
  ;; this works because there are no tokens that span multiple lines
  ;; in the input; it is advantages because it means that no cropping
  ;; is required (and the cropping is hard to get right)
  (define (tt s) ((current-code-tt) s))
  (define liness (for/list ([p (in-list paths)])
                   (define all-lines (file->lines p))
                   (define desired-lines
                     (for/list ([l (in-list all-lines)]
                                [i (in-naturals 1)]
                                #:when (<= first-line i last-line))
                       l))
                   (unless (= (length desired-lines)
                              (+ (- last-line first-line) 1))
                     (error 'program->figure*
                            "didn't find lines from ~a to ~a in ~a"
                            first-line last-line p))
                   (widen desired-lines)))
  (define hash-lang-lines
    (for/list ([p (in-list paths)])
      (call-with-input-file p
        (λ (port)
          (for/first ([l (in-list (port->lines port))]
                      #:when (regexp-match? #rx"#lang" l))
            l)))))
  (define digits (string-length (~a (- last-line first-line))))
  (define first-gaps #f)
  (define lines-with-numbers
    (apply
     vl-append
     (for/list ([lines (in-list (transpose liness))]
                [i (in-naturals 1)])
       (define gaps (build-list (length lines) (λ (_) (tt "  "))))
       (unless first-gaps (set! first-gaps gaps))
       (apply
        hbl-append
        buffer
        (for/list ([line (in-list lines)]
                   [gap (in-list gaps)]
                   [hash-lang-line (in-list hash-lang-lines)])
          (hbl-append
           (tt (~r #:min-width digits i))
           gap
           (codeblock-pict (string-append hash-lang-line "\n" line)
                           #:keep-lang-line? #f)))))))
  (for/fold ([p lines-with-numbers])
            ([first-gap (in-list first-gaps)])
    (define-values (x _) (cc-find lines-with-numbers first-gap))
    (pin-over p
              x 0
              (frame (blank 0 (pict-height lines-with-numbers))))))

(define (widen ls)
  (define widest (apply max (map string-length ls)))
  (for/list ([l (in-list ls)])
    (string-append l (make-string (- widest (string-length l)) #\space))))
(define (transpose l) (apply map list l))

(module+ test
  (require rackunit)
  (check-equal? (widen '("" "1" "12345" "123"))
                '("     "
                  "1    "
                  "12345"
                  "123  ")))

(define-runtime-path even-odd-prefix.rkt "../mini-java/even-odd-prefix.rkt")
(define-runtime-path expanded-even-odd.rkt "../mini-java/expanded-even-odd.rkt")
(define-runtime-path even-odd.rkt "../mini-java/even-odd.rkt")
(define mj-simple-example
  (codeblock-pict
   (port->string (open-input-file even-odd.rkt))))

(define parenthesized-mj-example
  (codeblock-pict
   (port->string (open-input-file even-odd-prefix.rkt))))

(define expanded-even-odd
  (codeblock-pict
   (port->string (open-input-file expanded-even-odd.rkt))))

(define expansion-figure
  (program->figure* (list even-odd-prefix.rkt expanded-even-odd.rkt)
                    #:first-line 10
                    #:last-line 72))

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

(define-runtime-path prefix-mini-java.rkt "../mini-java/prefix-mini-java.rkt")
(define-runtime-path infix-mini-java.rkt "../mini-java/infix-mini-java.rkt")
(define-runtime-path typecheck.rkt "../mini-java/typecheck.rkt")
(define-extract mj-new prefix-mini-java.rkt #:keep-lang-line? #f)
(define-extract typecheck-mod-beg infix-mini-java.rkt #:keep-lang-line? #f)
(define-extract add-tool-tips typecheck.rkt #:keep-lang-line? #f)

(define-runtime-path property.rkt "../editing/property.rkt")
(define refactor-impl
  (codeblock-pict
   (string-append
    (extract property.rkt 'refactor-prop)
    "\n"
    (extract prefix-mini-java.rkt 'refactor-if #:prefix-lang? #f))))

(define break-impl
  (codeblock-pict
   (string-append
    (extract prefix-mini-java.rkt 'break-param)
    "\n\n"
    (extract prefix-mini-java.rkt 'while+break #:prefix-lang? #f))))

(define mj-while-macro
  (codeblock-pict #:keep-lang-line? #f
   #<<>>
#lang racket
(define-syntax (while stx)
  (syntax-parse stx
    [(while test body ...)
     #`(letrec ([loop (λ ()
                        (when test
                          body ...
                          (loop)))])
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
  ;; very sad parameterize; Inconsolata appears not to have ╬ ╗ ╣ and friends
  (parameterize ([current-code-font '(bold . modern)])
    (codeblock-pict
     #:keep-lang-line? #f
     2d-state-machine-text)))

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
