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
                        
                        (do-edit-if outer-context
                                    if-loc
                                    test-loc
                                    then-loc
                                    else-loc))))))

              (define/private (do-edit-if ctx-loc if-loc test-loc then-loc else-loc)
                (match-define (list ctx-start ctx-span) (shift ctx-loc))
                (match-define (list if-start if-span) (shift if-loc))
                (match-define (list test-start test-span) (shift test-loc))
                (match-define (list then-start then-span) (shift then-loc))
                (match-define (list else-start else-span) (shift else-loc))
                (printf "STARTING EDIT ...\n")
                (begin-edit-sequence)
                (define ctx-pre (copy-to-temp-text ctx-start if-start))
                (define ctx-post (copy-to-temp-text (+ if-start if-span) (+ ctx-start ctx-span)))
                ;; delete the post context
                (send this delete (+ if-start if-span) (+ ctx-start ctx-span))
                ;; put the post context after the else ....
                (send ctx-post move/copy-to-edit
                      this
                      0
                      (- (+ ctx-start ctx-span) (+ if-start if-span))
                      (+ else-start else-span)
                      #:try-to-move? #f)
                ;; put the pre context before the else
                (send ctx-pre move/copy-to-edit
                      this
                      0
                      (- if-start ctx-start)
                      else-start
                      #:try-to-move? #f)
                ;; put the post context after the then
                (send ctx-post move/copy-to-edit
                      this
                      0
                      (- (+ ctx-start ctx-span) (+ if-start if-span))
                      (+ then-start then-span)
                      #:try-to-move? #f)
                ;; put the pre context before the then
                (send ctx-pre move/copy-to-edit
                      this
                      0
                      (- if-start ctx-start)
                      then-start
                      #:try-to-move? #f)
                ;; delete the pre context
                (send this delete ctx-start if-start)
                (end-edit-sequence))

              (define/private (copy-to-temp-text start end)
                (define temp (new (text:basic-mixin (editor:basic-mixin text%))))
                (send this move/copy-to-edit temp start end 0 #:try-to-move? #f)
                temp)

              (define/private (shift loc)
                (match-define (syntax-info _ pos span) loc)
                (list (sub1 pos) span))

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