#lang racket
(require pict images/logos slideshow pict/code ppict)
(require (prefix-in : (only-in pict/code code)))

(provide (all-defined-out))

(define WIDTH 500)
(define HEIGHT 375)
(define RADIUS 0.5)
(define INNER-DIAMETER 340)
(define INNER-RADIUS (/ INNER-DIAMETER 2))
(define RATIO (/ INNER-DIAMETER WIDTH))

(define (my-t label)
  (text label (current-main-font) (* 2 (current-font-size))))

(define (make-cloud label #:color [color "AliceBlue"] #:size [size (current-font-size)])
  (define background-cloud (cloud WIDTH HEIGHT "black"))
  (define the-cloud (cloud (- WIDTH 4) (- HEIGHT 4) color))
  (define the-label (text label (current-main-font) (* 2 size)))
  (cc-superimpose background-cloud the-cloud the-label))


(define (label str)
  (text str (current-main-font) (* 2 (current-font-size))))

(define the-cloud
  (cc-superimpose (cloud WIDTH HEIGHT "black")
                  (cloud (- WIDTH 4) (- HEIGHT 4) "AliceBlue")))

(define (my-tt str)
         (text str `(bold . modern)  (* 1.5 (current-font-size))))

(define SLIDE-W 1024)
(define SLIDE-H 768)

(define my-base-pict
  (inset
   (colorize (filled-rectangle SLIDE-W SLIDE-H) "Ivory")
   (- client-w SLIDE-W)
   (- client-h SLIDE-H)))

(define CODE-SCALE 1.85)
(define-syntax-rule (code . stx)
  (parameterize ([code-colorize-enabled #f])
    (scale
     (:code . stx)
     CODE-SCALE)))

(define (make-code-pict lang str #:scale [s CODE-SCALE])
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

(define (fold-fade/align picts aligner times)
  (define base-pict (first picts))
  (for/fold ([base base-pict])
            ([pict (in-list (rest picts))]
             [time (in-list times)])
    (fade-pict time base pict #:combine aligner)))


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
   (list "while" "||" "if" "define-class" "new" "send")
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
  ;(printf "n: ~a\n" n)
  ;(printf "from: ~a\n" tag-from)
  ;(printf "to: ~a\n" tag-to) 
  (define pict-path-from (find-tag base tag-from))
  ;(printf "pict-path-from : ~a\n" pict-path-from)
  (define pict-path-to (find-tag base tag-to))
  ;(printf "pict-path-to : ~a\n" pict-path-to)
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

(define (make-tagged-ghost str [ghost? #t] #:tag-suffix [post-tag "-end"])
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

(define (make-racket-pict str [colorize-enabled #t])
  (parameterize ([code-colorize-enabled colorize-enabled])
    (make-code-pict "" str)))

(define (compiler-define-class ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "define-class" ghost-binding? #:tag-suffix "mj") " c b ...) «code»]"))
(define (compiler-new ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "new" ghost-binding? #:tag-suffix "mj") " c)                «code»]"))
(define (compiler-send ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "send" ghost-binding? #:tag-suffix "mj") " o m a ...)       «code»]"))
(define (compiler-while ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "while" ghost-binding? #:tag-suffix "mj") " test body ...)  «code»]"))
(define (compiler-if-mj ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "if" ghost-binding? #:tag-suffix "mj") " test then else)    «code»]"))
(define (compiler-or-mj ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "or" ghost-binding? #:tag-suffix "mj") " e1 e2 ...)         «code»]"))

(require unstable/gui/pict)

(define (colored-or n)
  (make-compiler-line "[("
                      (fade-pict n
                                 (make-racket-pict "or")
                                 (backdrop (make-racket-pict "||") #:color "Lavender"))
                      " e1 e2 ...)"
                      "         «code»]"))

(define (pin-over* base dx dy picts)
  (for/fold ([base base])
            ([pict (in-list picts)])
    (pin-over base dx dy pict)))

(define mj-compiler
  (pin-over*
   (vl-append
    50
    (ghost (make-code-pict "" ""))
    (tg (ghost (compiler-define-class #f)) 'mj-def-class-e)
    (tg (ghost (compiler-send #f)) 'mj-send-e)
    (tg (ghost (compiler-new #f)) 'mj-new-e)
    (tg (ghost (compiler-while #f)) 'mj-while-e)
    (tg (ghost (compiler-if-mj #f)) 'mj-if-e)
    (tg (ghost (compiler-or-mj #f)) 'mj-or-e))
   0
   -200 ; FIXME: what's the right number here???
   (list (tg (ghost (compiler-define-class #f)) 'mj-def-class-s)
         (tg (ghost (compiler-send #f)) 'mj-send-s)
         (tg (ghost (compiler-new #f)) 'mj-new-s)
         (tg (ghost (compiler-while #f)) 'mj-while-s))))


(define (word-cloud label picts)
  (define label-pict (if (pict? label) label (my-t label)))
  (match-define (list p1 p2 p3 p4 p5 p6) picts)
  (ppict-do
   the-cloud
   #:go (coord .5 .33) label-pict
   #:go (coord .5 .17)
   (ppict-do
    (blank 220 80)
    #:go (coord .5 .5 'cc) p1)
   #:go (coord .5 .6)
   (ppict-do
    (blank 440 145)
    #:go (coord 0 .25 'lc) p2
    #:go (coord 1 .1 'rc) p3
    #:go (coord 0.2 0.5 'lc) p4
    #:go (coord 1 .75 'rt) p5)
   #:go (coord .5 .9)
   (ppict-do
    (blank 200 60)
    #:go (coord .5 .5 'cb) p6)))

(define CODE-BLOB-PICT (make-racket-pict "«code»"))

(define (make-code-map-pict pict)
  (define OPACITY .75)
  (define SCALE .75)
  (hc-append (make-racket-pict "⟨")
             pict
             (make-racket-pict " ↦ ")
             (scale (cellophane CODE-BLOB-PICT OPACITY) SCALE)
             (make-racket-pict "⟩")))


(define (fade-to-ghost p n)
  (fade-pict n p (ghost p)))
(define (fade-from-ghost p n)
  (fade-pict n (ghost p) p))

(define (transformer-cloud . times)
  (word-cloud-transition
    (fade-from-ghost (label "transformer") (first times))
    (list "racket" "typed/racket" "scribble" "racket" "racket")
    (list (list "letrec" "or" "if" "define" "lambda" "#%app")
          (list "All" ":" "ann" "define-type" "require/typed" "Listof")
          (list "defproc" "examples" "authors" "define-cite" "include-section" "code")
          (list "while" "||" "if" "define-class" "send" "new")
          (list "letrec" "or" "if" "define" "lambda" "#%app"))
    (rest times)))

(define (reader-cloud . times)
  (word-cloud-transition #:reader? #t
    (fade-from-ghost (label "reader") (first times))
    (list "racket" "typed/racket" "scribble" "mini-java" "racket")
    (list (list "#(1 2 3)" "(4 . < . 5)" "#hash()" "'(6 7 . 8)" "`(,(+ 9 10))" "#i10")
          (list "'(1 2)" "#{2 :: Integer}" "#{map @ String Integer}" "" "" "")
          (list "@~cite[plt-tr1]" "@title[#:tag \"mj\"]{Intro}" "" "@{Hello}"  "" "" )
          (list  "check.is_even(6)" "true || false" "class Main {...}" "" "" "")
          (list "#(1 2 3)" "(4 . < . 5)" "'(6 7 . 8)" "#hash()" "`(,(+ 9 10))" "#i10"))
    (rest times)))

(define (runtime-cloud . times)
  (word-cloud-transition
    (fade-from-ghost (label "runtime") (first times))
    (list "racket" "typed/racket" "scribble" "racket" "racket")
    (list (list "empty?" "+" "<=" "read" "vector" "cons")
          (list "empty?" "+" "<=" "read" "vector" "cons")
          (list "secref" "title" "cite" "figure" "make-bib" "bold")
          (list "index" "+" "!" "new-int-array" "length" "==")
          (list "empty?" "+" "<=" "read" "vector" "cons"))
    (rest times)))

(define (transpose lst) (apply map list lst))

(define (word-cloud-transition label langs wordss #:reader? [reader? #f] times)
  (define SCALE 2.25)
  (define ALIGNERS (list cc-superimpose lc-superimpose rc-superimpose lc-superimpose rt-superimpose cb-superimpose))
  (define prepare-pict (if reader? values make-code-map-pict))
  (define base (ppict-do the-cloud #:go (coord .5 .33) label))
  (define picts
    (for/list ([words (in-list (transpose wordss))]
               [aligner (in-list ALIGNERS)])
      (fade-from-ghost
       (fold-fade/align
        (for/list ([lang (in-list langs)]
                   [word (in-list words)])
          (prepare-pict (make-code-pict lang word #:scale SCALE)))
        aligner
        (rest times))
       (first times))))
  (word-cloud label picts))

(define final-reader (reader-cloud 1 1 1 1 1 1))
(define final-transformer (transformer-cloud 1 1 1 1 1 1))
(define final-runtime (runtime-cloud 1 1 1 1 1 1))



             
             
  