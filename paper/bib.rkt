#lang racket
(require scriblib/autobib)
(provide (all-defined-out))


(define ACM "ACM ")
(define Conference "Conf. ")
(define Workshop "Wksp. ")
(define Symposium "Sym. ")
(define icfp
  "Intl. Conference on Functional Programming")
(define lfp "LISP and Functional Programming")
(define pldi (string-append ACM Conference "Programming Language Design and Implementation"))
(define scheme-workshop (string-append Workshop "Scheme and Functional Programming"))
(define popl (string-append ACM Symposium "Principles of Programming Languages"))
(define esop (string-append "European " Symposium "on Programming"))

(define-cite ~cite citet generate-bibliography)

(define dynamic-typing
  (make-bib
   #:title "Dynamic Typing: Syntax and Proof Theory"
   #:author "Fritz Henglein"
   #:location (proceedings-location esop #:pages '(197 230))
   #:date 1992))


;; DrRacket
(define drracket
  (make-bib
   #:title "DrScheme: A Programming Environment for Scheme"
   #:author (authors "Robert Bruce Findler"
                     "John Clements"
                     "Cormac Flanagan"
                     "Matthew Flatt"
                     "Shriram Krishnamurthi"
                     "Paul Steckler"
                     "Matthias Felleisen")
   #:location (journal-location "Journal of Functional Programming"
                                #:pages '(159 182)
                                #:volume 12
                                #:number 2)
   #:date 2002))


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
   #:location (proceedings-location "Summit on Advances in Programming Languages")
   #:date 2015))

;;  - You want it when
(define you-want-it-when
  (make-bib
   #:author "Matthew Flatt"
   #:title "Composable and Compilable Macros: You Want it When?"
   #:location (proceedings-location
               icfp
               #:pages '(72 83))
   #:date 2002))

(define mtwt
  (make-bib
   #:title "Macros that Work Together: Compile-Time Bindings, Partial Expansion, and Definition Contexts"
   #:author (authors "Matthew Flatt" "Ryan Culpepper" "David Darais" "Robert Bruce Findler")
   #:location (journal-location "Journal of Functional Programming"
                                #:pages '(181 216)
                                #:volume 22
                                #:number 2)
   #:date 2012))
   
;;  - Languages as Libraries
(define langs-as-libs
  (make-bib
   #:title "Languages as Libraries"
   #:author (authors "Sam Tobin-Hochstadt" "Vincent St-Amour"
                     "Ryan Culpepper" "Matthew Flatt" "Matthias Felleisen")
   #:location (proceedings-location pldi #:pages '(132 141))
   #:date 2011))

;;  - Advanced macrology and impl ts
(define cthf-sfp-2007
  (make-bib
   #:title "Advanced Macrology and the Implementation of Typed Scheme"
   #:author (authors "Ryan Culpepper" "Sam Tobin-Hochstadt" "Matthew Flatt")
   #:location (proceedings-location scheme-workshop #:pages '(1 13))
   #:date 2007))

;;  - typed racket
(define tr-diss
  (make-bib
   #:title "Typed Scheme: From Scripts to Programs"
   #:author "Sam Tobin-Hochstadt"
   #:location (dissertation-location #:institution "Northeastern University"
                                     #:degree "Ph.D.")
   #:date 2010))

(define thf-popl-2008
  (make-bib
   #:title "The Design and Implementation of Typed Scheme"
   #:author (authors "Sam Tobin-Hochstadt" "Matthias Felleisen")
   #:location (proceedings-location popl
                                    #:pages '(395 406))
   #:date 2008))

;;  - syntax-parse
(define fortifying-macros
  (make-bib
   #:title "Fortifying Macros"
   #:author (authors "Ryan Culpepper" "Matthias Felleisen")
   #:location (proceedings-location
               icfp
               #:pages '(235 246))
   #:date 2010))

;;  - scribble/ algol60/datalog docs
(define scribble
  (make-bib
   #:title "Scribble: Closing the Book on Ad Hoc Documentation Tools"
   #:author (authors "Matthew Flatt" "Eli Barzilay" "Robert Bruce Findler")
   #:location (proceedings-location
               icfp
               #:pages '(109 120))
   #:date 2009))
   
;;  - syntax parameters
(define syntax-params
  (make-bib
   #:title "Keeping it Clean with Syntax Parameters"
   #:author (authors "Eli Barzilay" "Ryan Culpepper" "Matthew Flatt")
   #:location (proceedings-location scheme-workshop)
   #:date 2011))
   
;;  - sets of scopes?
(define scopes
  (make-bib
   #:title "Binding as Sets of Scopes"
   #:author "Matthew Flatt"
   #:location (proceedings-location
               popl
               #:pages '(705 717))
   #:date 2016))


;; readtables, FIXME: is this the right way to cite this?
(define readtables
  (make-bib
   #:title "MacLISP Reference Manual, Revision 0"
   #:author "David Moon"
   #:location "M.I.T. Project MAC"
   #:date 1974
   #:url "http://www.softwarepreservation.org/projects/LISP/MIT/Moon-MACLISP_Reference_Manual-Apr_08_1974.pdf"))

(define commonlisp
  (make-bib
   #:title "An Overview of COMMON LISP"
   #:author "Guy L. Steele, Jr."
   #:location (proceedings-location lfp #:pages '(98 107))
   #:date 1982))

;;  - Racket TR1
(define plt-tr1
  (make-bib
   #:title    "Reference: Racket"
   #:author   (authors "Matthew Flatt" "PLT")
   #:date     "2010"
   #:location (techrpt-location #:institution "PLT Inc." #:number "PLT-TR-2010-1")
   #:url "http://racket-lang.org/tr1/"))



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

;; redex book
(define redex-book
  (make-bib
    #:author (authors "Matthias Felleisen" "Robert Bruce Findler" "Matthew Flatt")
    #:title "Semantics Engineering with PLT Redex"
    #:location (book-location #:publisher "MIT Press")
    #:is-book? #t
    #:date "2010"))

;; hygienic macros
(define hygienic-macros
  (make-bib #:title "Hygienic Macro Expansion"
            #:author (authors "Eugene Kohlbecker"
                              "Daniel P. Friedman"
                              "Matthias Felleisen"
                              "Bruce Duba")
            #:location (proceedings-location lfp #:pages '(151 161))
            #:date 1986))
(define fsp
  (make-bib
   #:title "Feature-Specific Profiling"
   #:author (authors "Vincent St-Amour" "Leif Andersen" "Matthias Felleisen")
   #:location (proceedings-location "International Conference on Compiler Construction"
                                     #:pages '(49 68))
   #:date 2015))

(define syntactic-abstraction-in-scheme
  (make-bib #:title "Syntactic Abstraction in Scheme"
            #:author (authors "R. Kent Dybvig" "Robert Hieb" "Carl Bruggeman")
            #:date 1992
            #:location (journal-location "Lisp and Symbolic Computation"
                                         #:volume 5
                                         #:number 4)))

(define macros-that-work
  (make-bib #:title "Macros that Work"
            #:date 1991
            #:author (authors "William Clinger" "Jonathan Rees")
            #:location (proceedings-location popl)))