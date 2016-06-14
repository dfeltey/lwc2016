#lang racket

(require drracket/tool
         framework
         racket/gui
         racket/runtime-path
         "context-table.rkt"
         "syntax-info.rkt")

(provide tool@)

(define-runtime-path context-table.rkt "context-table.rkt")

(define tool@
  (unit (import drracket:tool^)
        (export drracket:tool-exports^)
        
        (define (phase1) (void))
        (define (phase2) (void))

        (define make-refactor-text%
          (λ (super%)
            (class super%
              (inherit begin-edit-sequence end-edit-sequence
                       insert delete)
              
              (field [context-table #f]
                     [refactor-table #f]
                     [source #f])

              (define/private (update-context-table! [table #f])
                (set! context-table table))
              (define/private (update-refactor-table! [table #f])
                (set! refactor-table table))
              (define/private (update-source! [src #f])
                (set! source src))

              
              (define/public (refactor-callback expanded-info)
                (printf "updating fields ...\n")
                (cond
                  [expanded-info
                   (match-define (list source context-table refactor-table) expanded-info)
                   (printf "source is ~a\n" source)
                   (update-context-table! context-table)
                   (update-refactor-table! refactor-table)
                   (update-source! source)]
                  [else
                   (update-context-table!)
                   (update-refactor-table!)
                   (update-source!)]))

              (define/public (refactor-build-popup-menu menu pos text)
                (when (and context-table refactor-table)
                  (make-object menu-item%
                    "Lift if expression" menu
                    (λ (item evt) (do-lift-if pos)))
                  (void)))

              (define/private (do-lift-if pos)
                (printf "lifting if ...\n")
                (printf "pos is ~a\n" pos)
                (when (and context-table refactor-table)
                  (printf "tables non-false ...\n")
                  (printf "refactor-table: ~a\n" refactor-table)
                  (define partial-info (syntax-info source pos #f))
                  (printf "partial-info is ~a\n" partial-info)
                  (define refactor-info (hash-ref refactor-table partial-info #f))
                  (when refactor-info
                    (printf "found refactor info ...\n")
                    (match-define (cons span refactor-prop) refactor-info)
                    (when refactor-prop
                      (match-define (list if-loc test-loc then-loc else-loc) refactor-prop)
                      (define loc (syntax-info source pos span))
                      (define outer-context
                        (find-surrounding-context context-table loc))
                      (when outer-context
                        (printf "found outer context ...\n")
                        (match-define (syntax-info _ position span) outer-context)

                        (define full-context
                          (send this get-text (sub1 position) (sub1 (+ position span))))
                        (define replacement
                          (lift-if-string full-context
                                          if-loc
                                          test-loc
                                          then-loc
                                          else-loc
                                          position))
             
                        (printf "full-context: ~a\n" full-context)
                        (begin-edit-sequence)
                        (printf "in edit sequence ...\n")
                        (send this delete (sub1 position) (sub1 (+ position span)))
                        (send this insert replacement (sub1 position))
                        (end-edit-sequence))))))

              (define/private (lift-if-string str if-loc test-loc then-loc else-loc offset)
                (printf "str: ~a\nif-loc: ~a\ntest: ~a\nthen: ~a\nelse:~a\noffset:~a\n"
                         str if-loc test-loc then-loc else-loc offset)
                (match-define (list if-start if-span) (shift if-loc offset))
                (match-define (list test-start test-span) (shift test-loc offset))
                (match-define (list then-start then-span) (shift then-loc offset))
                (match-define (list else-start else-span) (shift else-loc offset))

                (define (fill-context s)
                  (define pre (substring str 0 if-start))
                  (define post (substring str (+ if-start if-span)))
                  (string-append pre s post))

                (string-append
                 (substring str if-start then-start)
                 (fill-context (substring str then-start (+ then-start then-span)))
                 (substring str (+ then-start then-span) else-start)
                 (fill-context (substring str else-start (+ else-start else-span)))
                 (substring str (+ else-start else-span) (+ if-start if-span))))

#|
"(+ 1 (if x 2 3))"
5 10
9 1
11 1
13 1

|#
                
                
              (define/private (shift loc offset)
                (match-define (syntax-info _ pos span) loc)
                (list (- pos offset) span))

                      
              (super-new))))
        
        
        (keymap:add-to-right-button-menu/before
         (let ([old (keymap:add-to-right-button-menu/before)])
           (λ (menu editor event)
             (old menu editor event)
             (define-values (pos text) (send editor get-pos/text event))
             (when (and pos (is-a? text text%))
               (send editor refactor-build-popup-menu menu pos text)))))

        (drracket:get/extend:extend-definitions-text make-refactor-text%)
        (drracket:module-language-tools:add-online-expansion-handler
         context-table.rkt
         'handle-expansion
         (λ (text expanded-info)
           (printf "calling refactor-callback after expansion returns ...\n")
           (send text refactor-callback expanded-info)))
        
        ))