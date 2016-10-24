#lang slideshow

(require ppict
         "racket-lwc-pict.rkt"
         "util.rkt"
         (except-in pict/code code)
         slideshow/play
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
 (λ (n1 n2 n21 n22 n23 n3 n4 n5 n6)
   (define (word-cloud label)
     (ppict-do
      the-cloud
      #:go (coord .5 .33)
      (fade-from-ghost (my-t label) n1)))
   (define center-cloud-pos (launder (ghost the-cloud)))
   (define reader-cloud-pos (launder (ghost the-cloud)))
   (define runtime-cloud-pos (launder (ghost the-cloud)))
   (define transformer-cloud-pos (launder (ghost the-cloud)))
   (define the-runtime-cloud (runtime-cloud n1 n22 n3 n4 n5 n6))
   (define the-transformer-cloud (transformer-cloud n1 n23 n3 n4 n5 n6))
   (define the-reader-cloud (reader-cloud n1 n21 n3 n4 n5 n6))
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
      #:go (coord .25 .5)
      reader-cloud-pos
      #:go (coord .75 .24)
      transformer-cloud-pos
      #:go (coord .75 .76)
      runtime-cloud-pos))
   (for/fold ([base base-pict])
             ([pict (in-list (append (list the-runtime-cloud the-reader-cloud the-transformer-cloud lang-pict)))]
              [pict-end (in-list (list runtime-cloud-pos
                                       reader-cloud-pos
                                       transformer-cloud-pos
                                       lang-end))])
     (slide-pict/center base pict center-cloud-pos pict-end n1)))
 #:skip-last? #t)

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
      #:go (coord -.3 -.3 'ct) lang-end
      #:go (coord .25 .5) (tg nothing 'reader-start)
      #:go (coord -.5 .5) (tg nothing 'reader-end)
      #:go (coord .75 .24) (tg nothing 'transformer-start)
      #:go (coord .5 .5) (tg nothing 'transformer-end)
      #:go (coord .75 .76) (tg nothing 'runtime-start)
      #:go (coord 1.5 .76) (tg nothing 'runtime-end))
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
    ))
 #:skip-last? #t)

(play-n
 (λ (n1 n2)
   (define words (list "letrec" "or" "if" "define" "lambda" "#%app"))
   (define start-tags
     (map (λ (str) (string->symbol (string-append str "-start"))) words))
   (define end-tags
     (map (λ (str) (string->symbol (string-append str "-end"))) words))
   (define code-start-tags
     (map (λ (str) (string->symbol (string-append str "-code-start"))) words))
   (define code-end-tags
       (map (λ (str) (string->symbol (string-append str "-code-end"))) words))
   (define word-picts (map (λ (str) (make-code-pict "" str #:scale STARTING-SCALE)) words))
   (define code-picts
     (map (λ (_) (scale
                  (cellophane CODE-BLOB-PICT (translate CODE-STARTING-OPACITY 1.0 n2))
                  (translate CODE-STARTING-SCALE 1 n2)))
          words))
   (slide-picts/tags
    (ppict-do
     my-base-pict
     #:go (coord .5 .5)
     (transformer-to-compiler n1 n2 n2))
    (append word-picts code-picts)
    (append start-tags code-start-tags)
    (append end-tags code-end-tags)
    n2))
 #:skip-last? #t)


(require racket/draw)

(play-n
 (let ()
   (define racket-box (freeze (filled-rounded-rectangle 480 700 #:color "bisque")))
   (define java-box (freeze (filled-rounded-rectangle 480 700 #:color (invert-color "bisque"))))
   (λ (n1 n2)
   (slide-picts/tags
    (ppict-do
     my-base-pict
     #:go (coord .5 .5)
     (scale the-cloud 4)
     #:go (coord .5 .5)
     (fade-to-ghost (compiler #t #t) n1)
     #:go (tile 2 1)
     (cc-superimpose
      (fade-from-ghost
       racket-box
       n1)
      (vc-append
       50
       (tg (ghost (compiler-define #t)) 'defl-e)
       (tg (ghost (compiler-lambda #t)) 'laml-e)
       (tg (ghost (compiler-app #t)) 'appl-e)
       (tg (ghost (compiler-letrec #t)) 'letl-e)
       (tg (ghost (compiler-if #t)) 'ifl-e)
       (tg (ghost (compiler-or #t)) 'orl-e)
       ))
     (cc-superimpose
      (fade-from-ghost
       java-box
       n2)
      mj-compiler))
    (list (compiler-define #f)
          (compiler-lambda #f)
          (compiler-app #f)
          (compiler-letrec #f)
          (compiler-if #f)
          (compiler-or #f))
    '(defl laml appl letl ifl orl)
    '(defl-e laml-e appl-e letl-e ifl-e orl-e)
    n1)))
 #:skip-last? #t)

(play-n
 (let ()
   (define cd (tg (compiler-define #f) 'defl-e))
   (define cla (tg (compiler-lambda #f) 'laml-e))
   (define cap (tg (compiler-app #f) 'appl-e))
   (define cle (tg (compiler-letrec #f) 'letl-e))
   (define cif (tg (compiler-if #f) 'ifl-e))
   (define cor (tg (compiler-or #f) 'orl-e))
   (define racket-box (freeze (filled-rounded-rectangle 480 700 #:color "bisque")))
   (define java-box (freeze (filled-rounded-rectangle 480 700 #:color (invert-color "bisque"))))
   (define shift-if (backdrop (inset (compiler-if #f) 4 6) #:color "bisque"))
 (λ (n1 n2 n3 n4 n5 n6)
   (define shift-or (backdrop (inset (colored-or n2) 4 6) #:color "bisque"))
   (fade-pict
    n6
    (slide-picts/tags
    (slide-pict/tags
     (slide-picts/tags
      (ppict-do
       my-base-pict
       #:go (coord .5 .5)
       (scale the-cloud 4)
       #:go (tile 2 1)
       (cc-superimpose
        racket-box 
        (vc-append
         50
         cd
         cla
         cap
         cle
         cif
         cor
         ))
       (cc-superimpose
        java-box
        mj-compiler))
      (list
       shift-if
       shift-or)
      '(ifl-e orl-e)
      '(mj-if-e mj-or-e)
      n1)
     (compiler-while #f n4)
     'mj-while-s
     'mj-while-e
     n3)
    (list
     (compiler-define-class #f)
     (compiler-send #f)
     (compiler-new #f))
    '(mj-def-class-s mj-send-s mj-new-s)
    '(mj-def-class-e mj-send-e mj-new-e)
    n5
    )
    (let ([p (blank client-w client-h)])
      (refocus (cc-superimpose (colorize (filled-rectangle 1024 768) "Ivory") p) p)))))
 #:skip-last? #t)

(start-at-recent-slide)
(play-n
 (λ (n1 n2 n3 n31 n4 n5 n6)
 (ppict-do
  my-base-pict
  #:go (coord .5 .1)
  (fade-from-ghost (titlet "The 2016 Language Workbench Challenge") n6)
  #:go (coord .5 .5)
  (pins
   (fade-to-ghost (scale (mini-mini-java n31 n4) .8) n6)
   (list (fade-to-ghost (fade-from-ghost (wrap lang-line) n1) n2)
         (fade-to-ghost (fade-from-ghost (wrap require-block) n2) n3)
         (fade-to-ghost (fade-from-ghost (wrap (provide-block n31 n4)) n3) n4)
         (fade-to-ghost (fade-from-ghost (wrap while-block) n4) n5))
   '(lang require provide while))))
 #:skip-last? #t)

(play-n
 (λ (n1 n2 n3)
   (ppict-do
    my-base-pict
    #:go (coord .5 .1)
    (titlet "The 2016 Language Workbench Challenge")
    #:go (coord .25 .3 'lc)
    (fade-from-ghost (item "Tabular Notation") n1)
    #:go (coord .25 .5 'lc)
    (fade-from-ghost (item "Beyond-Grammar Restrictions") n2)
    #:go (coord .25 .7 'lc)
    (fade-from-ghost (item "Restructuring") n3))))
    

   




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