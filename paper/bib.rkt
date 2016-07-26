#lang racket
(require scriblib/autobib)
(provide (all-defined-out))

(define-cite ~cite citet generate-bibliography)

;; TODO: citations
;;  - MiniJava
(define mini-java
  (make-bib #:author "Eric Roberts"
            #:title "An Overview of MiniJava"
            #:location (proceedings-location "SIGCSE"
                                             #:pages '(1 5)
                                             #:series 32)
            #:date 2001))

;;  - Racket manifesto
(define manifesto
  (make-bib
   #:author (authors "Matthias Felleisen"
                     "Robert Bruce Findler"
                     "Matthew Flatt"
                     "Shriram Krishnamurthi"
                     "Eli Barzilay"
                     "Jay McCarthy"
                     "Sam Tobin-Hochstadt")
   #:title "The Racket Manifesto"
   #:location (proceedings-location "Summit on Advances in Programming Languages"
                                    )))

;;  - You want it when
;;  - Languages as Libraries
;;  - Advanced macrology and impl ts
;;  - syntax-parse
;;  - typed racket
;;  - scribble/ algol60/datalog docs
;;  - syntax parameters
;;  - sets of scopes?
;;  - Racket TR1
;;  - readtables??? docs???
;;  - dave herman blog post
;;  - Visual Studio Code, Lnaguage server protocol

(define mcilroy
  (make-bib #:author "Doug McIlroy"
            #:title "Macro Instruction Extensions of Compiler Languages"
            #:location (journal-location "Communications of the ACM"
                                         #:volume 3
                                         #:number 4)
            #:date 1960))

(define lisp-macros
  (make-bib #:author "Timothy P. Hart"
            #:title "MACRO Definitions for LISP"
            #:date 1963
            #:location (techrpt-location #:institution "MIT" #:number "57")))
