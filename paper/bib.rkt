#lang racket
(require scriblib/autobib)
(provide (all-defined-out))

(define-cite ~cite citet generate-bibliography)

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
