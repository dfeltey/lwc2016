#lang slideshow

(require ppict
         "picts.rkt"
         "pslide-staged.rkt"
         "util.rkt"
         pict/code
         slideshow/text
         slideshow/play
         images/logos
         pict)
 (require unstable/gui/pict/plt-logo
          unstable/gui/pict)

(require ppict-slide-grid)
;(pslide-base-pict (thunk (colorize (filled-rectangle client-w client-h) "Ivory")))
(set-grid-base-pict!)


;; LWC Talk Outline

;; Title
(pslide
 #:go (coord .5 .5)
 (make-plt-title-background 1024 768)
 #:go (coord 0.5 0.33)
 (t "Languages the Racket Way"))

(define (fade-to-ghost p n)
  (fade-pict n p (ghost p)))
(define (fade-from-ghost p n)
  (fade-pict n (ghost p) p))

(play-n
 #:skip-last? #t
(λ (n1 n2)
  (define syntax (fade-pict n2 (label "syntax") (label "reader")))
  (define semantics (fade-pict n2 (label "semantics") (label "bindings")))
  (define syntax-ghost (launder (ghost (make-cloud "syntax"))))
  (define semantics-ghost (launder (ghost (make-cloud "semantics"))))
  (define syntax-cloud (cc-superimpose the-cloud syntax))
  (define semantics-cloud (cc-superimpose the-cloud semantics))
  (define base (fade-to-ghost PLT-LOGO-PICT n1))
  (define syntax-slide (fade-from-ghost syntax-cloud n1))
  (define semantics-slide (fade-from-ghost semantics-cloud n1))
  (slide-pict/center
   (slide-pict/center
    (ppict-do
     my-base-pict
      #:go (coord .5 .5)
      base
      #:go (coord .5 .1)
      (fade-to-ghost (titlet "The Racket Approach to Programming Languages") n1)
      #:go (tile 2 1)
      syntax-ghost
      semantics-ghost
      #:go (coord .49 .1 'rc)
      (fade-from-ghost (my-tt "#lang") n2))
    syntax-slide
    base
    syntax-ghost
    n1)
   semantics-slide
   base
   semantics-ghost
   n1)))

(play-n
 (λ (n1 n2 n3 n4 n5)
   (define langs (apply fold-fade
                        (map my-tt (list "" "racket" "typed/racket" "scribble" "mini-java" "racket"))
                        (list n1 n2 n3 n4 n5)
                        #:combine lc-superimpose))
   (define empty-cloud (blank WIDTH HEIGHT))
   (define bindings
     (fold-fade (list empty-cloud racket-bindings typed-racket-bindings scribble-bindings mj-bindings racket-bindings) n1 n2 n3 n4 n5))
   (define readings
     (fold-fade (list empty-cloud racket-readers typed-racket-readers scribble-readers mj-readers racket-readers) n1 n2 n3 n4 n5))
   (ppict-do
    my-base-pict
    #:go (tile 2 1)
    (cc-superimpose (make-cloud "reader") readings)
    (cc-superimpose (make-cloud "bindings") bindings)
    #:go (coord .49 .1 'rc)
    (my-tt "#lang")
    #:go (coord .51 .1 'lc)
    langs))
 #:skip-last? #t)


(play-n
(λ (n1 n2)
  (define (racket-code-pict str) (make-code-pict "racket" str))
  (define bad (ghost (blank WIDTH HEIGHT)))
  (define ghost-reader (launder (ghost (blank WIDTH HEIGHT))))
  (define ghost-reader-o (launder (ghost (blank WIDTH HEIGHT))))
  (define ghost-bindings (launder (ghost (blank WIDTH HEIGHT))))
  (define ghost-bindings-c (launder (ghost (blank WIDTH HEIGHT))))
  (define bad-reader
    (cc-superimpose
     (cc-superimpose (make-cloud "reader") racket-readers)
     (fade-pict n1 bad
                (cross-out bad #:width 5 #:color "red"))))
  (slide-pict/center
   (slide-pict/center
   (ppict-do
    my-base-pict
    #:go (tile 2 1)
    ghost-reader
    ghost-bindings
    #:go (coord .49 .1 'rc)
    (my-tt "#lang")
    #:go (coord .51 .1 'lc)
    (my-tt "racket")
    #:go (coord .5 .5)
    ghost-bindings-c
    #:go (coord -0.33 -0.33)
    ghost-reader-o)
   (cc-superimpose (make-cloud "bindings") racket-bindings)
   ghost-bindings
   ghost-bindings-c
   n2)
   bad-reader
   ghost-reader
   ghost-reader-o
   n2))
#:skip-last? #t)

(play-n
 (λ (n1)
   (ppict-do
    my-base-pict
    #:go (coord .5 .5)
    (cc-superimpose (scale the-cloud (add1 (* n1 3))) (fade-to-ghost (label "bindings") n1) racket-bindings)
    #:go (coord .49 .1 'rc)
    (fade-to-ghost (my-tt "#lang") n1)
    #:go (coord .51 .1 'lc)
    (fade-to-ghost (my-tt "racket") n1)

   )))




;; Programming Languages
;; Reader + Bindings (#lang)
;; (Cross out Reader) Just talking about the bindings
;; Explode the bindings/compiler (#lang racket)
;; Reuse/Modify/New/Subtract Bindings to get #lang mini-java
;; sliding boxes from #lang racket impl to #lang mini-java impl
;; #lang mini-java
;; The benchmarks we solve
;; Demo (2 parts)
;; Demo (Part 1)
;; A Simple MiniJava Program (No extensions)
;; A MiniJava program with 2d-syntax
;; Applying the refactoring to a MiniJava program
;; An example of break
;; Demo (Part 2) [Deeper Dive into `while` and `break`]
;; Program using `while` (use macro stepper???)
;; `break` outside a `while` loop just errors
;; Program using `break` inside while (macro stepper??)
;; Nested `while` loops show that `break` breaks only the inner loop
;; Implementation of while and break/interposition on break/syntax parameters