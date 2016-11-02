#lang racket
(require pict slideshow pict/code ppict racket/draw)
(require (prefix-in : (only-in pict/code code)))

(provide (all-defined-out))

(define WIDTH 500)
(define HEIGHT 375)
(define RADIUS 0.5)
(define INNER-DIAMETER 340)
(define INNER-RADIUS (/ INNER-DIAMETER 2))
(define RATIO (/ INNER-DIAMETER WIDTH))

(define (invert-color c)
  (define orig (make-object color% c))
  (define MAX 255)
  (make-object color%
    (- MAX (send orig red))
    (- MAX (send orig green))
    (- MAX (send orig blue))))

(define-syntax-rule (invert-code-color p)
  (let ()
    (define (inv c) (invert-color c))
    (parameterize ([current-id-color (inv (current-id-color))]
                   [current-comment-color (inv (current-comment-color))]
                   [current-base-color (inv (current-base-color))]
                   [current-literal-color (inv (current-literal-color))]
                   [current-keyword-color (inv (current-keyword-color))])
      p)))
       

(define STARTING-SCALE 2.1) ;; was 2.25 ???
(define CODE-STARTING-OPACITY .75)
(define CODE-STARTING-SCALE .75) ;; OLD : (* .75 (/ 1.85 2))

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
   (/ (- client-w SLIDE-W) 2)
   (/ (- client-h SLIDE-H) 2)))

(define CODE-SCALE 1.85)
(define-syntax-rule (code . stx)
  (parameterize ([code-colorize-enabled #f])
    (scale
     (:code . stx)
     CODE-SCALE)))

(define (make-code-pict lang str #:scale [s STARTING-SCALE])
    (define the-lang
      (if (string=? "" lang) "racket" lang))
    (scale
     (codeblock-pict #:keep-lang-line? #f
                     (string-append "#lang " the-lang "\n" str))
     s))

(define racket-binding-picts
  (map (λ (str) (make-code-pict "racket" str))
       (list "letrec" "or" "if" "var:id" "lambda" "#%app")))

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
    [else (make-code-pict "" str #:scale STARTING-SCALE)]))

(define (make-tagged-ghost str [ghost? #t] #:tag-suffix [post-tag "-end"])
  (define do-ghost (if ghost? ghost values))
  (tag-pict (do-ghost (make-code-pict "" str)) (string->symbol (string-append str post-tag))))

(define (make-compiler-element binding extra ghost-binding? #:tag-suffix [tag-suffix "-end"])
  (define do-ghost (if ghost-binding? ghost values))
  (make-compiler-line
   "[("
   (tg (do-ghost (make-code-pict "" binding #:scale STARTING-SCALE)) (string->symbol (string-append binding tag-suffix)))
   extra
   (tg (do-ghost (make-code-pict "" "«code»" #:scale STARTING-SCALE)) (string->symbol (string-append binding "-code" tag-suffix)))
   "]"))

(define (compiler-define ghost-binding?)
  (define do-ghost (if ghost-binding? ghost values))
  (make-compiler-line
   "["
   (tg (do-ghost (make-code-pict "" "var:id" #:scale STARTING-SCALE)) (string->symbol (string-append "var:id" "-end")))
   "              "
   (tg (do-ghost (make-code-pict "" "«code»" #:scale STARTING-SCALE)) (string->symbol (string-append "var:id" "-code" "-end")))
   "]"))
#;(make-compiler-element "define" " x e)        " ghost-binding?) 
#;(make-compiler-line "[(" (make-tagged-ghost "define" ghost-binding?) " x e)            ""«code»]")
(define (compiler-lambda ghost-binding?)
  (make-compiler-element "lambda" " (x ...) e)  " ghost-binding?)
  #;(make-compiler-line "[(" (make-tagged-ghost "lambda" ghost-binding?) " (x ...) e)      ""«code»]"))
(define (compiler-app ghost-binding?)
  (make-compiler-element "#%app" " a b ...)     " ghost-binding?)
  #;(make-compiler-line "[(" (make-tagged-ghost "#%app" ghost-binding?) " a b ...)         ""«code»]"))
(define (compiler-letrec ghost-binding?)
  (make-compiler-element "letrec" " ([x e]) b)  " ghost-binding?)
  #;(make-compiler-line "[(" (make-tagged-ghost "letrec" ghost-binding?) " ([x e] ...) b)  ""«code»]"))
(define (compiler-if ghost-binding?)
  (make-compiler-element "if" " test then else) " ghost-binding?)
  #;(make-compiler-line "[(" (make-tagged-ghost "if" ghost-binding?) " test then else)     ""«code»]"))
(define (compiler-or ghost-binding?)
  (make-compiler-element "or" " e1 e2 ...)      " ghost-binding?)
  #;(make-compiler-line "[(" (make-tagged-ghost "or" ghost-binding?) " e1 e2 ...)          ""«code»]"))

;; rough pict for open compiler slides
(define (compiler ghost-binding? [ghost-line? #f])
  (define do-ghost (if ghost-line? ghost values))
  (vl-append (make-code-pict "" "(define-syntax (compile exp)" #:scale STARTING-SCALE)
             (make-code-pict "" "  (syntax-parse exp" #:scale STARTING-SCALE)
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
   (list "var:id" "lambda" "#%app" "letrec" "if" "or")))
(define racket-compiler-tags-starts
  (map string->symbol (list "var:id" "lambda" "#%app" "letrec" "if" "or")))
(define racket-compiler-tags-ends
  (map
   (λ (str) (string->symbol (string-append str "-end")))
   (list "var:id" "lambda" "#%app" "letrec" "if" "or")))

(define (make-racket-pict str [colorize-enabled #t])
  (parameterize ([code-colorize-enabled colorize-enabled])
    (make-code-pict "" str)))

(define (compiler-define-class ghost-binding?)
  (invert-code-color
   (make-compiler-element "class"" c b ...)     " ghost-binding? #:tag-suffix "mj"))
  #;(make-compiler-line "[(" (make-tagged-ghost "define-class" ghost-binding? #:tag-suffix "mj") " c b ...) «code»]"))
(define (compiler-new ghost-binding?)
  (invert-code-color
   (make-compiler-element "new"" c)             " ghost-binding? #:tag-suffix "mj"))
  #;(make-compiler-line "[(" (make-tagged-ghost "new" ghost-binding? #:tag-suffix "mj") " c)                «code»]"))
(define (compiler-send ghost-binding?)
  (invert-code-color
   (make-compiler-element "send"" o m a ...)    " ghost-binding? #:tag-suffix "mj"))
  #;(make-compiler-line "[(" (make-tagged-ghost "send" ghost-binding? #:tag-suffix "mj") " o m a ...)       «code»]"))
;(backdrop (invert-code-color (make-racket-pict "||")) #:color (invert-color "Bisque"))
(define (bd p) (backdrop p #:color "Bisque"))

(define (compiler-while ghost-binding? [n 0])
  (define p1
    (invert-code-color
     (make-compiler-element "while"" tst exp ...) " ghost-binding? #:tag-suffix "mj")))
  (define p2
    (vc-append
              (invert-code-color (make-compiler-line "[(while tst exp ...)        "))
              (bd (make-compiler-line "  #'(letrec ([loop           "))
              (bd (make-compiler-line "              (λ ()          "))
              (bd (make-compiler-line "                (when tst    "))
              (bd (make-compiler-line "                  exp ...    "))
              (bd(make-compiler-line "                  (loop)))]) "))
              (hc-append (bd (make-compiler-line "      (loop))")) (invert-code-color (make-racket-pict "]               ")))))
  (refocus
   (fade-pict n
              (ct-superimpose p1 (ghost p2))
              (ct-superimpose (ghost p1) p2)
              #:combine ct-superimpose)
   p1))
#;(make-compiler-line "[(" (make-tagged-ghost "while" ghost-binding? #:tag-suffix "mj") " test body ...)  «code»]")
(define (compiler-if-mj ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "if" ghost-binding? #:tag-suffix "mj") " test then else)    «code»]"))
(define (compiler-or-mj ghost-binding?)
  (make-compiler-line "[(" (make-tagged-ghost "or" ghost-binding? #:tag-suffix "mj") " e1 e2 ...)         «code»]"))

(require unstable/gui/pict)

(define (colored-or n)
  (make-compiler-line "[("
                      (fade-pict n
                                 (make-racket-pict "or")
                                 (backdrop (invert-code-color (make-racket-pict "||")) #:color (invert-color "Bisque")))
                      " e1 e2 ...)"
                      "      «code»]"))

(define (pin-over* base dx dy picts)
  (for/fold ([base base])
            ([pict (in-list picts)])
    (pin-over base dx dy pict)))

;(define (ins p) (inset p 0 50))

(define mj-compiler
  (pin-over*
   (vc-append
    50
    ;(ghost (compiler-define-class #f))
    (tg (ghost (compiler-define-class #f)) 'mj-def-class-e)
    ;(tg (ghost (compiler-send #f)) 'mj-send-e)
    ;(tg (ghost (compiler-new #f)) 'mj-new-e)
    (tg (ghost (compiler-while #f)) 'mj-while-e)
    (tg (ghost (compiler-send #f)) 'mj-send-e2)
    (tg (ghost (compiler-new #f)) 'mj-new-e2)
    (tg (ghost (compiler-define-class #f)) 'mj-var-e)
    (tg (ghost (compiler-if-mj #f)) 'mj-if-e)
    (tg (ghost (compiler-or-mj #f)) 'mj-or-e)
    #;(ghost (compiler-define-class #f)))
   0
   -200 ; FIXME: what's the right number here???
   (list (tg (ghost (compiler-define-class #f)) 'mj-def-class-s)
         (tg (ghost (compiler-send #f)) 'mj-send-s)
         (tg (ghost (compiler-new #f)) 'mj-new-s)
         (tg (ghost (compiler-while #f)) 'mj-while-s))))


(define (word-cloud label picts #:base-pict [base-pict the-cloud])
  (define label-pict (if (pict? label) label (my-t label)))
  (match-define (list p1 p2 p3 p4 p5 p6) picts)
  (ppict-do
   base-pict
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
    (fade-from-ghost (label "transformers") (first times))
    (list "racket" "typed/racket" "scribble" "datalog" "lazy" "racket" "racket")
    (list (list "letrec" "or" "if" "var:id" "lambda" "#%app")
          (list "All" ":" "ann" "define-type" "require/typed" "Listof")
          (list "defproc" "examples" "authors" "define-cite" "include-section" "code")
          (list "datalog" "~" ":-" "?" "datalog!" "!") ; datalog
          (list "let" "when" "if" "set!" "begin" "#%app") ; lazy
          (list "while" "||" "if" "class" "send" "new")
          (list "letrec" "or" "if" "var:id" "lambda" "#%app"))
    (rest times)))

(define (reader-cloud . times)
  (word-cloud-transition #:reader? #t
    (fade-from-ghost (label "reader") (first times))
    (list "racket" "typed/racket" "scribble" "racket" "lazy" "racket" "racket")
    (list (list "#(1 2 3)" "(4 . < . 5)" "#hash()" "'(6 7 . 8)" "`(,(+ 9 10))" "#i10")
          (list "'(1 2)" "#{2 :: Integer}" "#{map @ String Integer}" "'(6 7 . 8)" "`(,(+ 9 10))" "#i10")
          (list "@~cite[plt-tr1]"
                ""
                "@title[#:tag \"mj\"]{Intro}"
                "\n@p{Lorem ipsum dolor\nsit amet, consectetur\nadipiscing elit, sed}" ;\nadipiscing elit,\nsed do eiusmod tempor incididunt ut labore et dolore magna aliqua."
                ""
                "")
          (list "parent(jon, joe)"
                ""
                "path(X, Y) :- edge(X, Y)."
                "ancestor(A, B) :-\n    parent(A, C),\n    ancestor(C, B)"
                ""
                "") ; datalog
          (list "#(1 2 3)" "(! (begin (print \"Hi\") 45))" "#hash()" "'(6 7 . 8)" "(let ([x (loop)]) 9)" "#i10"); lazy
          (list  "check.is_even(6)" "true || false" "class Main {...}" "primes.length;" "i = i + 1;" "primes[2];")
          (list "#(1 2 3)" "(4 . < . 5)" "'(6 7 . 8)" "#hash()" "`(,(+ 9 10))" "#i10"))
    (rest times)))

(define (runtime-cloud . times)
  (word-cloud-transition
    (fade-from-ghost (label "runtime") (first times))
    (list "racket" "typed/racket" "scribble" "racket" "racket" "racket" "racket")
    (list (list "empty?" "+" "<=" "read" "vector" "cons")
          (list "empty?" "+" "<=" "read" "vector" "cons")
          (list "secref" "title" "cite" "figure" "make-bib" "bold")
          (list "theory/c" "make-theory" "write-theory" "read-theory" "retract!" "assume!") ; datalog
          (list "!list" "!!" "!" "vector" "!!list" "cons") ; lazy
          (list "index" "+" "!" "new-int-array" "length" "==")
          (list "empty?" "+" "<=" "read" "vector" "cons"))
    (rest times)))

(define (transpose lst) (apply map list lst))

(define (word-cloud-transition label langs wordss #:reader? [reader? #f] times)
  (define ALIGNERS (list cc-superimpose lc-superimpose rc-superimpose lc-superimpose rt-superimpose cb-superimpose))
  (define prepare-pict (if reader? values make-code-map-pict))
  (define picts
    (for/list ([words (in-list (transpose wordss))]
               [aligner (in-list ALIGNERS)])
      (fade-from-ghost
       (fold-fade/align
        (for/list ([lang (in-list langs)]
                   [word (in-list words)])
          (prepare-pict (make-code-pict lang word #:scale STARTING-SCALE)))
        aligner
        (rest times))
       (first times))))
  (word-cloud label picts))

(define final-reader (reader-cloud 1 1 1 1 1 1 1 1))
(define final-transformer (transformer-cloud 1 1 1 1 1 1 1 1))
(define final-runtime (runtime-cloud 1 1 1 1 1 1 1 1))



(define (opening-transformer n1 n2)
  (define words (list "letrec" "or" "if" "var:id" "lambda" "#%app"))
  (define (prepare-pict word)
    (fade-to-ghost
     (hc-append (make-racket-pict "⟨")
                (tg (ghost (make-racket-pict word)) (string->symbol (string-append word "-start")))
                (make-racket-pict " ↦ ")
                (tg (ghost (scale (cellophane CODE-BLOB-PICT CODE-STARTING-OPACITY) CODE-STARTING-SCALE)) (string->symbol (string-append word "-code-start")))
                (make-racket-pict "⟩"))
     n2))
  (define picts (map prepare-pict words))
  (word-cloud
   (fade-to-ghost (my-t "transformers") n1)
   picts
   #:base-pict (ghost the-cloud)))

(define (transformer-to-compiler n1 n2 n3)
  (cc-superimpose
   (scale the-cloud (translate 1 4 n1))
   (opening-transformer n1 n2)
   (fade-from-ghost (compiler #t) n3)))

#;
(define (compiler2 ghost-binding? [ghost-line? #f])
  (define (code-pict str) (make-code-pict "racket" str #:scale STARTING-SCALE))
  (define do-ghost (if ghost-line? ghost values))
  (vl-append (code-pict "(define-syntax (compile exp)")
             (code-pict "  (syntax-parse exp")
             (make-compiler-line "    " (tg (do-ghost (compiler-define ghost-binding?)) 'defl))
             (make-compiler-line "    " (tg (do-ghost (compiler-lambda ghost-binding?)) 'laml))
             (make-compiler-line "    " (tg (do-ghost (compiler-app ghost-binding?)) 'appl))
             (make-compiler-line "    " (tg (do-ghost (compiler-letrec ghost-binding?)) 'letl))
             (make-compiler-line "    " (tg (do-ghost (compiler-if ghost-binding?)) 'ifl))
             (hc-append
              (make-compiler-line "    " (tg (do-ghost (compiler-or ghost-binding?)) 'orl))
              (code-pict "))"))))


(define (pins base picts tags)
  (for/fold ([base base])
            ([tag (in-list tags)]
             [pict (in-list picts)])
    (define path (find-tag base tag))
    (define-values (x y) (cc-find base path))
    (pin-over/align base x y 'c 'c pict)))

(define (buffer-x p) (+ 10 (pict-width p)))
(define (buffer-y p) (+ 3 (pict-height p)))
(define (wrap p)
  (cellophane
   (ellipse (buffer-x p) (buffer-y p) #:border-width 10 #:border-color "red")
   .75))



(define lang-line
  (tg (scale (codeblock-pict "#lang racket/base" #:keep-lang-line? #t) STARTING-SCALE) 'lang))

  
(define require-block
  (tg (vl-append (make-racket-pict "(require (for-syntax syntax/parse")
               (make-racket-pict "                     racket/base)")
               (make-racket-pict "         racket/bool)")
               (make-racket-pict ""))
    'require))



(define while-block
  (tg (vl-append (make-racket-pict "(define-syntax (while stx)")
               (make-racket-pict "  (syntax-parse stx")
               (make-racket-pict "    [(while test:expr body ...)")
               (make-racket-pict "     #`(letrec ([loop (λ ()")
               (make-racket-pict "                        (when test")
               (make-racket-pict "                          body ...")
               (make-racket-pict "                          (loop)))])")
               (make-racket-pict "         (loop))]))"))
    'while))

(define (mini-mini-java n1 n2)
(vl-append
lang-line
(make-racket-pict "")
require-block
(provide-block n1 n2)

(make-racket-pict "")
while-block
(make-racket-pict "")))

(define (fade-in-out p n1 n2)
  (fade-to-ghost (fade-from-ghost p n1) n2))
(define (wrap-code w h n1 n2)
  (fade-in-out
   (cellophane
    (rectangle w h #:border-width 10 #:border-color "red")
    .75)
   n1
   n2))

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

(define MJ-IMPL-PICT (freeze (make-mj-pict)))

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
   (translate .001 .8 n1))
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
     (translate .001 .8 n1))
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
     (translate .001 .8 n1))
    #:background-color "ivory")
   (translate 0 1 n1)))

(define pre-if
(vl-append
 (make-code-pict "mini-java" "if (test) {")
 (make-code-pict "mini-java" "    then_branch;")
 (make-code-pict "mini-java" "} else {")
 (make-code-pict "mini-java" "    else_branch;")
 (make-code-pict "mini-java" "}")))

(define post-if
(vl-append
 (make-code-pict "mini-java" "if (! (test)) {")
 (make-code-pict "mini-java" "    else_branch;")
 (make-code-pict "mini-java" "} else {")
 (make-code-pict "mini-java" "    then_branch;")
 (make-code-pict "mini-java" "}")))

(define refactor-pict
  (let* ([pre (inset pre-if 20)]
         [post (inset post-if 20)]
         [combined (hc-append 100 pre post)])
    (pin-arrow-line 20 combined
                    pre rc-find
                    post lc-find
                    #:color "red"
                    #:line-width 3)))

  