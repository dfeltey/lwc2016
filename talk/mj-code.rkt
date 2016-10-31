#lang racket

(require slideshow slideshow/code unstable/gui/slideshow ppict slideshow/play)

(require (except-in "util.rkt" code))

(provide implementation-slides)

(define (make-mj-pict)
    (define files
      (append
       (map (λ (str) (string-append "../mini-java/" str))
            (list "lexer-sig.rkt"
                  "parser-sig.rkt"
                  "tokens.rkt"
                  "lexer-unit.rkt"
                  "parser-unit.rkt"
                  "error.rkt"
                  "typecheck.rkt"
                  "state-machine-classes.rkt"
                  "prefix-mini-java.rkt"
                  "main.rkt"))
       (map (λ (str) (string-append "../editing/" str))
            (list "property.rkt"
                  "syntax-info.rkt"
                  "if-refactor.rkt"
                  "context-table.rkt"
                  "refactor-tool.rkt"))))
      (define CODES
        (map (compose codeblock-pict file->string) files))
      (define MAX-HEIGHT (apply max (map pict-height CODES)))
      (define MAX-WIDTH (apply max (map pict-width CODES)))
      (define INSET-PICTS
        (for/list ([p (in-list CODES)])
          (scale
           (inset p
                  (/ (- MAX-WIDTH (pict-width p)) 2)
                  0 #;(/ (- MAX-HEIGHT (pict-height p)) 2))
           (/ (client-h) MAX-HEIGHT))))

  (match-define (list lex-sig parse-sig tokens
                      lexer parser error
                      typecheck state-machines
                      mini-java main property
                      syntax-info if-refactor
                      context-table tool)
    INSET-PICTS)
  

  (define (top-half p)
    (define half (blank (pict-width p)
                        (/ (pict-height p) 2)))
    (clip (refocus (ct-superimpose p half) half)))
  (define (bottom-half p)
    (define half (blank (pict-width p)
                        (/ (pict-height p) 2)))
    (clip (refocus (cb-superimpose p half) half)))
  (define lex-pict (vl-append lex-sig tokens lexer))
  (define parse-pict (vl-append parse-sig parser))
  (define mini-java-pict (vl-append mini-java state-machines main))
  (define tool-pict (vl-append property syntax-info if-refactor context-table tool))
  (define typecheck-top (top-half typecheck))
  (define typecheck-bot (bottom-half typecheck))
  (define PRE-SCALED
    (list lex-pict parse-pict typecheck-top typecheck-bot mini-java-pict tool-pict))

  (define MAX-HEIGHT2
    (apply max (map pict-height PRE-SCALED)))
  (apply hc-append
         30
         (for/list ([p (in-list PRE-SCALED)])
           (scale p (/ (client-h) MAX-HEIGHT2)))))

(define (fade-in-out p n1 n2)
  (fade-to-ghost (fade-from-ghost p n1) n2))
(define (wrap-code w h n1 n2)
  (fade-in-out
   (cellophane
    (rectangle w h #:border-width 10 #:border-color "red")
    .75)
   n1
   n2))

(define (provide-block n1 n2)
  (cellophane
   (shadow-frame
    (scale
    (vl-append (make-racket-pict "(provide (all-defined-out)")
               (make-racket-pict "         true false < + - *")
               (hc-append(make-racket-pict "         (rename-out ")
                         (fade-around-pict n2
                                           (make-racket-pict "[displayln     System.out.println]")
                                           (λ (p) (backdrop p #:color "yellow"))))
               (make-racket-pict "                     [set!          =]")
               (make-racket-pict "                     [eqv?          ==]")
               (make-racket-pict "                     [vector-set!   array=]")
               (make-racket-pict "                     [and           &&]")
               (hc-append (make-racket-pict "                     ")
                          (make-racket-pict"[or            ||]"))
               (make-racket-pict "                     [vector-ref    index]")
               (make-racket-pict "                     [vector-length length]")
               (make-racket-pict "                     [not           !]")
               (make-racket-pict "                     [modulo        %]")
               (make-racket-pict "                     [make-vector   new-int-array]))"))
   (translate .001 .3 n1))
    #:background-color "ivory")
   (translate 0 1 n1)))

(define (parser-block n1)
  (cellophane
   (shadow-frame
    (scale
     (vl-append
      (make-racket-pict "(ClassDeclaration")
      (make-racket-pict "     [(class Identifier ClassBody)")
      (make-racket-pict "      (to-syntax `(class ,$2 { ,@$3 })")
      (make-racket-pict "                 (src->list (build-src 3)))]")
      (make-racket-pict "     [(class Identifier extends Identifier ClassBody)")
      (make-racket-pict "      (to-syntax `(class ,$2 extends ,$4{ ,@$5 })")
      (make-racket-pict "                 (src->list (build-src 5)))]")
      (make-racket-pict "     [(2D)")
      (make-racket-pict "      (to-syntax $1")
      (make-racket-pict "                 (src->list (build-src 1)))])"))
     (translate .001 .3 n1))
    #:background-color "ivory")
   (translate 0 1 n1)))

(define (transformer-block n1)
  (cellophane
   (shadow-frame
    (scale
     (vl-append
      (make-racket-pict "(define-syntax (while stx)")
      (make-racket-pict "  (syntax-parse stx")
      (make-racket-pict "    [(while test:expr body ...)")
      (make-racket-pict "     #`(let/ec local-break")
      (make-racket-pict "         (syntax-parameterize ([break (λ (stx) #'(local-break))])")
      (make-racket-pict "           (letrec ([loop (λ ()")
      (make-racket-pict "                            (when test")
      (make-racket-pict "                              body ...")
      (make-racket-pict "                              (loop)))])")
      (make-racket-pict "             (loop))))]))"))
     (translate .001 .3 n1))
    #:background-color "ivory")
   (translate 0 1 n1)))
    

;(slide mj-code)
;(pslide (make-mj-pict))
(define MJ-IMPL-PICT (freeze (make-mj-pict)))
(define (implementation-slides)
  (play-n
   (λ (n1  n12 n2 n3 n32 n33 n4 n5 n52 n6)
     (slide-pict/tags
      (slide-pict/tags
       (slide-pict/tags
        (ppict-do
         my-base-pict
         #:go (coord .5 .5) (tg MJ-IMPL-PICT 'end)
         #:go (coord .17 .5)
         (tg (wrap-code (* .34 (client-w)) (client-h) n1 n2) 'reader-start)
         #:go (coord .7 .1)
         (tg (wrap-code (* .15 (client-w)) (* .15 (client-h)) n3 n4) 'runtime-start)
         #:go (coord .34 .14 'lt)
         (tg (wrap-code (* .45 (client-w)) (* .87 (client-h)) n5 n6) 'transformer-start))
        (fade-to-ghost (parser-block n12) n2)
        'reader-start
        'end
        n12)
       (fade-to-ghost (provide-block n32 n33) n4)
       'runtime-start
       'end
       n32)
      (fade-to-ghost (transformer-block n52) n6)
      'transformer-start
      'end
      n52))))
