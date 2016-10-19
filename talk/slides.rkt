#lang slideshow

(require ppict
         "picts.rkt"
         "pslide-staged.rkt"
         "racket-lwc-pict.rkt"
         "util.rkt"
         (except-in pict/code code)
         slideshow/text
         slideshow/play
         images/logos
         pict)
 (require unstable/gui/pict/plt-logo
          unstable/gui/pict)

(require ppict-slide-grid)
;(pslide-base-pict (thunk (colorize (filled-rectangle client-w client-h) "Ivory")))
;(set-grid-base-pict!)


;; LWC Talk Outline

;; Title
(pslide
 #:go (coord .5 .5)
 (make-plt-title-background 1024 768)
 #:go (coord 0.5 0.33)
 (t "Languages the Racket Way"))


(pslide
 #:go (coord 0.5 0.5)
 my-base-pict
 #:go (coord 0.5 0.5)
 giant-duck-pict)

(play-n
 (λ (n1 n2 n3 n4 n5 n6)
   (define (word-cloud label)
     (ppict-do
      the-cloud
      #:go (coord .5 .33)
      (fade-from-ghost (my-t label) n1)))
   (define center-cloud-pos (launder (ghost the-cloud)))
   (define reader-cloud-pos (launder (ghost the-cloud)))
   (define runtime-cloud-pos (launder (ghost the-cloud)))
   (define transformer-cloud-pos (launder (ghost the-cloud)))
   (define the-runtime-cloud (runtime-cloud n1 n2 n3 n4 n5 n6))
   (define the-transformer-cloud (transformer-cloud n1 n2 n3 n4 n5 n6))
   (define the-reader-cloud (reader-cloud n1 n2 n3 n4 n5 n6))
   (define lang-pict
     (fold-fade
      (map
       (λ (str) (my-tt (string-append "#lang " str)))
       '("" "racket" "typed/racket" "scribble" "mini-java" "racket"))
      #:combine lc-superimpose
      n2
      n3
      n4
      n5
      n6))
   (define lang-start (launder (ghost lang-pict)))
   (define lang-end (launder (ghost lang-pict)))
   (define base-pict
     (ppict-do
      my-base-pict
      #:go (coord 0 0 'lt)
      lang-end
      #:go (coord .5 .5)
      (cc-superimpose
       center-cloud-pos
       lang-start)
      #:go (coord .24 .5)
      reader-cloud-pos
      #:go (coord .76 .22)
      transformer-cloud-pos
      #:go (coord .76 .78)
      runtime-cloud-pos))
   (for/fold ([base base-pict])
             ([pict (in-list (append (list the-runtime-cloud the-reader-cloud the-transformer-cloud lang-pict)))]
              [pict-end (in-list (list runtime-cloud-pos
                                       reader-cloud-pos
                                       transformer-cloud-pos
                                       lang-end))])
     (slide-pict/center base pict center-cloud-pos pict-end n1)))
 #:skip-last? #t)
(start-at-recent-slide)
(play-n
 (λ (n1)
   (define lang (my-tt "#lang racket"))
   (define lang-start (launder (ghost lang)))
   (define lang-end (launder (ghost lang)))
   (define nothing (blank 0 0))
   (slide-pict
    (slide-picts/tags
     (ppict-do
      my-base-pict
      #:go (coord 0 0 'lt) lang-start
      #:go (coord .5 0 'ct) lang-end
      #:go (coord .24 .5) (tg nothing 'reader-start)
      #:go (coord -0.4 -0.5) (tg nothing 'reader-end)
      #:go (coord .76 .22) (tg nothing 'transformer-start)
      #:go (coord .5 .5) (tg nothing 'transformer-end)
      #:go (coord .76 .78) (tg nothing 'runtime-start)
      #:go (coord 1.42 1.78) (tg nothing 'runtime-end))
     (list final-reader
           final-runtime
           final-transformer)
     '(reader-start runtime-start transformer-start)
     '(reader-end runtime-end transformer-end)
     n1)
    lang
    lang-start
    lang-end
    n1
    )))







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
 (λ (n1 n2)
   (slide-picts/tags
    (ppict-do
     my-base-pict
     #:go (coord .5 .5)
     (cc-superimpose (scale the-cloud (add1 (* n1 3)))
                     (fade-from-ghost (compiler #t) n2)
                     (fade-to-ghost (label "bindings") n1)
                     (ghost racket-bindings)
                     )
     #:go (coord .49 .1 'rc)
     (fade-to-ghost (my-tt "#lang") n1)
     #:go (coord .51 .1 'lc)
     (fade-to-ghost (my-tt "racket") n1)
     #:go (coord .5 .1)
     (fade-from-ghost (titlet "An Open Compiler") n2)
     )
    racket-compiler-picts
    racket-compiler-tags-starts
    racket-compiler-tags-ends
    n2))
 #:skip-last? #t)

(play-n
 (λ (n1 n2)
   (slide-picts/tags
    (ppict-do
     my-base-pict
     #:go (coord .5 .5)
     (scale the-cloud 4)
     #:go (coord .5 .1)
     (titlet "An Open Compiler")
     #:go (coord .5 .5)
     (fade-to-ghost (compiler #t #t) n1)
     #:go (coord .25 .9)
     (fade-from-ghost (t "Racket") n1)
     #:go (coord .75 .9)
     (fade-from-ghost (t "MiniJava") n2)
     #:go (tile 2 1)
     (cc-superimpose
      (fade-from-ghost (file-icon 450 525 "bisque") n1)
      (vl-append
       50
       (ghost (make-code-pict "" ""))
       (tg (ghost (compiler-define #t)) 'defl-e)
       (tg (ghost (compiler-lambda #t)) 'laml-e)
       (tg (ghost (compiler-app #t)) 'appl-e)
       (tg (ghost (compiler-letrec #t)) 'letl-e)
       (tg (ghost (compiler-if #t)) 'ifl-e)
       (tg (ghost (compiler-or #t)) 'orl-e)))
     (cc-superimpose
      (fade-from-ghost (file-icon 450 525 "Lavender") n2)
      mj-compiler))
    (list (compiler-define #f)
          (compiler-lambda #f)
          (compiler-app #f)
          (compiler-letrec #f)
          (compiler-if #f)
          (compiler-or #f))
    '(defl laml appl letl ifl orl)
    '(defl-e laml-e appl-e letl-e ifl-e orl-e)
    n1))
 #:skip-last? #t)

(play-n
 (λ (n1 n2 n3)
   (slide-pict/tags
    (slide-picts/tags
     (ppict-do
      my-base-pict
      #:go (coord .5 .5)
      (scale the-cloud 4)
      #:go (coord .5 .1)
      (titlet "An Open Compiler")
      #:go (coord .25 .9)
      (t "Racket")
      #:go (coord .75 .9)
      (t "MiniJava")
      #:go (tile 2 1)
      (cc-superimpose
       (file-icon 450 525 "bisque")
       (vl-append
        50
        (ghost (make-code-pict "" ""))
        (tg (compiler-define #f) 'defl-e)
        (tg (compiler-lambda #f) 'laml-e)
        (tg (compiler-app #f) 'appl-e)
        (tg (compiler-letrec #f) 'letl-e)
        (tg (compiler-if #f) 'ifl-e)
        (tg (compiler-or #f) 'orl-e)))
      (cc-superimpose
       (file-icon 450 525 "Lavender")
       mj-compiler))
     (list
      (backdrop (inset (compiler-if-mj #f) 10 15) #:color "bisque")
      (backdrop (inset (colored-or n2) 10 15) #:color "bisque"))
     '(ifl-e orl-e)
     '(mj-if-e mj-or-e)
     n1)
    (compiler-while #f)
    'mj-while-s
    'mj-while-e
    n3)))

   




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