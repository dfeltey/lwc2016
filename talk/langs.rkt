#lang racket
(require slideshow)
(require "util.rkt")

(define LANGS
  (list "mzscheme"
        "racket/unit"
        "#%kernel"
        "racket/gui"
        "racket/signature"
        "racket/base"
        "2d"
        "scribble/doc"
        "scribble/base"
        "scripple/lp"
        "typed/racket"
        "typed/racket/no-check"
        "htdp/isl+"
        "htdp/bsl"
        "htdp/asl"
        "deinprogram/DMdA"
        "frtime"
        "r6rs"
        "r5rs"
        "srfi/provider"
        "swindle/base"
        "swindle/turbo"
        "web-server/insta"
        "web-server"
        "meta/web"
        "setup/infotab"
        "rascal"
        "pop-pl"
        "syntax/module-reader"
        "cur"
        "plai"
        "algol60"
        "datalog"
        "scribble"
        "lazy"
        "mini-java"
        "medic"
        "pollen"
        "quad"
        "dssl"
        "scribble/manual"
        "profj/full"
        "plai-lazy"
        "plai-typed"
        "minipascal"
        "parenlog"
        "turnstile"
        "thesis"
        "lipics"))

(define COUNT (length LANGS))
(define WIDTH (/ (client-w) 7))
(define HEIGHT (/ (client-h) 7))
(define RAW-PICTS (map tt LANGS))
(define MAX-HEIGHT (apply max (map pict-height RAW-PICTS)))
(define MAX-WIDTH (apply max (map pict-width RAW-PICTS)))
(define PICTS
  (for/list ([p (in-list RAW-PICTS)])
    (cellophane
     (colorize
      (scale
       p
       #;(inset p
              (/ (- MAX-WIDTH (pict-width p)) 2)
              (/ (- MAX-HEIGHT (pict-height p)) 2))
       (* 2 (/ WIDTH MAX-WIDTH)))
      "blue")
     .75)))
  
(define LOCS
  (sort
   (for*/list ([i (in-range 7)]
               [j (in-range 7)])
     (cons (/ i 7)
           (if (even? i)
               (/ (+ j .5) 7)
               (/ j 7))
               ))
   (λ (p1 p2)
     (match-define (cons y1 x1) p1)
     (match-define (cons y2 x2) p2)
     (or (< x1 x2) (and (= x1 x2) (< y1 y2))))))

(define PICT-LOCS (shuffle (map list PICTS LOCS)))

(provide PICT-LOCS COUNT)