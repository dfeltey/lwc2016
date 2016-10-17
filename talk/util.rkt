#lang racket
(require pict images/logos slideshow pict/code ppict)

(provide (all-defined-out))

(define HEIGHT 450)
(define WIDTH 450)
(define RADIUS 0.5)
(define INNER-DIAMETER 340)
(define INNER-RADIUS (/ INNER-DIAMETER 2))
(define RATIO (/ INNER-DIAMETER WIDTH))

(define PLT-LOGO-PICT
  #;(blank client-w client-h)
  (freeze (bitmap (plt-logo #:height 450))))

(define (make-cloud label #:color [color "AliceBlue"] #:size [size (current-font-size)])
  (define background-cloud (cloud (+ WIDTH 4) (+ HEIGHT 4) "black"))
  (define the-cloud (cloud WIDTH HEIGHT color))
  (define the-label (text label (current-main-font) (* 2 size)))
  (cc-superimpose background-cloud the-cloud the-label))


(define (label str)
  (text str (current-main-font) (* 2 (current-font-size))))

(define the-cloud
  (cc-superimpose (cloud (+ WIDTH 4) (+ HEIGHT 4) "black")
                  (cloud WIDTH HEIGHT "AliceBlue")))

(define (my-tt str)
         (text str `(bold . modern)  (* 1.5 (current-font-size))))

(define my-base-pict
  (colorize (filled-rectangle client-w client-h) "Ivory"))

#;(define (lang-pict lang reader-exs binding-exs)
  (define (my-tt str)
    (text str `(bold . modern)  (* 1.5 (current-font-size))))
  (define (make-code-pict str #:scale [s 1.5])
    (scale
     (codeblock-pict #:keep-lang-line? #f
                     (string-append "#lang " lang "\n" str))
     s))
  (match-define (list r1 r2) (map make-code-pict reader-exs))
  (match-define (list b1 b2 b3) (map make-code-pict binding-exs))
  (pslide
   #:go (tile 2 1)
   (ppict-do
    (make-cloud "reader")
    #:go (coord .5 .33)
    r1
    #:go (coord .5 .75)
    r2)
   (ppict-do
    (make-cloud "bindings")
    #:go (coord .25 .33)
    (rotate b1 (/ pi 6))
    #:go (coord .75 .33)
    (rotate b2 (- (/ pi 6)))
    #:go (coord .5  .75)
    b3)
   #:go (coord .51 .1 'lc)
   (my-tt lang)
   #:go (coord .49 .1 'rc)
   (my-tt "#lang")))

(define (make-code-pict lang str #:scale [s 1.85])
    (define the-lang
      (if (string=? "" lang) "racket" lang))
    (scale
     (codeblock-pict #:keep-lang-line? #f
                     (string-append "#lang " the-lang "\n" str))
     s))

(define racket-binding-picts
  (map (λ (str) (make-code-pict "racket" str))
       (list "letrec" "or" "if" "define" "lambda" "#%app")))

(define (point angle)
  (cons (* .5 (cos angle))
        (* .5 (sin angle))))

(define (angle->pos angle [factor 1])
  (define mag (* factor RADIUS RATIO))
  (define polar (make-polar mag angle))
  (cons (- .5 (imag-part polar))
        (- .5 (real-part polar))))

;; (listof pict?) -> pict?
;; places 6 picts around a cloud pict
(define (place-words-in-cloud picts [cloud the-cloud] [radii-lst #f] [aligns #f])
  (define len (length picts))
  (define radii (if radii-lst radii-lst (make-list len 1)))
  (define alignments (if aligns aligns (make-list len 'cc)))
  (define angle (/ (* 2 pi) len))
  (for/fold ([base cloud])
            ([i (in-range len)]
             [pict (in-list picts)]
             [radius (in-list radii)]
             [alignment (in-list alignments)])
    (match-define (cons x y) (angle->pos (* i angle) radius))
    (ppict-do
     base
     #:go (coord x y alignment)
     pict)))


;; (non-empty-listof pict?) (listof (real-in 0.0 1.0)) -> pict?
;; assumption: (length picts) == (add1 (length times))
(define (fold-fade picts #:combine [combine cc-superimpose] . times)
  (define base-pict (first picts))
  (for/fold ([base base-pict])
            ([pict (in-list (rest picts))]
             [time (in-list times)])
    (fade-pict time base pict #:combine combine)))


(define (make-bindings-pict lang words [radii #f] [aligns #f] #:tag? [tag? #f])
  (define picts (map (λ (str) (make-code-pict lang str)) words))
  (define tagged-picts
    (if tag?
        (map tag-pict picts (map string->symbol words))
        picts))
   (place-words-in-cloud
     tagged-picts
     (blank WIDTH HEIGHT)
     radii
     aligns))

;; BINDINGS PICTS
(define racket-bindings
  (make-bindings-pict
   "racket"
   (list "letrec" "or" "if" "define" "lambda" "#%app")
   (list 1 .9 .9 1 1 .9)
   #:tag? #t))

(define typed-racket-bindings
  (make-bindings-pict
   "typed/racket"
   (list "require/typed" "ann" "All" "define-type" ":" "Listof")
   (list 1 .9 .9 1 1 .9)))

(define scribble-bindings
  (make-bindings-pict
   "scribble"
   (list "secref" "title" "cite" "figure" "make-bib" "bold")
   (list 1 .9 .9 1 1 .9)))

;; using racket lexer because otherwise lexing looks weird
(define mj-bindings
  (make-bindings-pict
   "racket"
   (list "while" "or" "if" "define-class" "new" "send")
   (list 1 .9 .9 1 1 .9)))

;; READER PICTS
(define racket-readers
  (make-bindings-pict
   "racket"
   (list "#(1 2 3)" "(4 . < . 5)" "#hash()")
   (list .45 .7 1)
   (list 'ct 'cb 'rt)))

(define typed-racket-readers
  (make-bindings-pict
   "typed/racket"
   (list "#{map @ String Integer}" "#{2 :: Integer}" "'(1 2)")
   (list .45 .7 1)
   (list 'ct 'cb 'rt)))

(define scribble-readers
  (make-bindings-pict
   "scribble"
   (list "@section[#:tag \"mj\"]{MiniJava}" "@{Hello}" "@~cite[plt-tr1]")
   (list .45 .7 1)
   (list 'ct 'rb 'rt)))

(define mj-readers
  (make-bindings-pict
   "mini-java"
   (list  "check.is_even(6)" "true || false" "class Main {...}")
   (list .45 .7 1)
   (list 'ct 'cb 'rt)
   ))

(define (translate x1 x2 n)
  (+ x1 (* n (- x2 x1))))


(define (slide-pict/tags base p tag-from tag-to n #:find [find cc-find])
  (define pict-path-from (find-tag base tag-from))
  (define pict-path-to (find-tag base tag-to))
  (define-values (x-from y-from) (find base pict-path-from))
  (define-values (x-to y-to) (find base pict-path-to))
  (pin-over/align
   base
   (translate x-from x-to n)
   (translate y-from y-to n)
   'c
   'c
   p))

(define (slide-picts/tags base picts tags-from tags-to n)
  (for/fold ([base base])
            ([p (in-list picts)]
             [tag-from (in-list tags-from)]
             [tag-to (in-list tags-to)])
    (slide-pict/tags base p tag-from tag-to n)))


(define (tg p t) (tag-pict p t))

(define (make-compiler-line . lst)
  (apply hc-append (map make-compiler-clause lst)))

(define (make-compiler-clause str)
  (cond
    [(pict? str) str]
    [else (make-code-pict "" str)]))

(define (make-tagged-ghost str [ghost? #t] [post-tag "-end"])
  (define do-ghost (if ghost? ghost values))
  (tag-pict (do-ghost (make-code-pict "" str)) (string->symbol (string-append str post-tag))))

(define (compiler-define ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "define" ghost-binding?) " x e)           «code»]"))
(define (compiler-lambda ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "lambda" ghost-binding?) " (x ...) e)     «code»]"))
(define (compiler-app ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "#%app" ghost-binding?) " a b ...)        «code»]"))
(define (compiler-letrec ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "letrec" ghost-binding?) " ([x e] ...) b) «code»]"))
(define (compiler-if ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "if" ghost-binding?) " test then else)    «code»]"))
(define (compiler-or ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "or" ghost-binding?) " e1 e2 ...)         «code»]"))

;; rough pict for open compiler slides
(define (compiler ghost-binding? [ghost-line? #f])
  (define do-ghost (if ghost-line? ghost values))
  (vl-append (make-code-pict "" "(define-syntax (compile exp)")
             (make-code-pict "" "  (syntax-parse exp")
             (make-compiler-line "    " (tg (do-ghost (compiler-define ghost-binding?)) 'defl))
             (make-compiler-line "    " (tg (do-ghost (compiler-lambda ghost-binding?)) 'laml))
             (make-compiler-line "    " (tg (do-ghost (compiler-app ghost-binding?)) 'appl))
             (make-compiler-line "    " (tg (do-ghost (compiler-letrec ghost-binding?)) 'letl))
             (make-compiler-line "    " (tg (do-ghost (compiler-if ghost-binding?)) 'ifl))
             (hc-append
              (make-compiler-line "    " (tg (do-ghost (compiler-or ghost-binding?)) 'orl))
              (make-code-pict "" "))"))))

(define racket-compiler-picts
  (map
   (λ (str) (make-code-pict "" str))
   (list "define" "lambda" "#%app" "letrec" "if" "or")))
(define racket-compiler-tags-starts
  (map string->symbol (list "define" "lambda" "#%app" "letrec" "if" "or")))
(define racket-compiler-tags-ends
  (map
   (λ (str) (string->symbol (string-append str "-end")))
   (list "define" "lambda" "#%app" "letrec" "if" "or")))
