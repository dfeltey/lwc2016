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

              (define context-table #f)
              (define refactor-table #f)
              (define source #f)

              (define/private (update-context-table! [table #f])
                (set! context-table table))
              (define/private (update-refactor-table! [table #f])
                (set! refactor-table table))
              (define/private (update-source! [src #f])
                (set! source src))

              (define/public (refactor-callback expanded-info)
                (cond
                  [expanded-info
                   (match-define (list source context-table refactor-table) expanded-info)
                   (update-context-table! context-table)
                   (update-refactor-table! refactor-table)
                   (update-source! source)]
                  [else
                   (update-context-table!)
                   (update-refactor-table!)
                   (update-source!)]))

              (define/public (refactor-build-popup-menu menu pos text)
                (define refactor-menu
                  (make-object menu%
                    "Refactor"
                    menu))
                (send refactor-menu enable #f)
                (when (lift-refactoring-available? pos)
                  (new menu-item%
                       [label "Lift if expression"]
                       [parent refactor-menu]
                       [callback (λ (item evt) (do-lift-if pos))])
                  (send refactor-menu enable #t))
                (when (swap-then-else-refactoring-available? pos)
                  (new menu-item%
                       [label "Swap then/else branches"]
                       [parent refactor-menu]
                       [callback (λ (item evt) (do-swap-then-else pos))])
                  (send refactor-menu enable #t)))

              (define/private (get-raw-refactor-info pos)
                (and source
                     refactor-table
                     (hash-ref refactor-table (syntax-info source pos #f) #f)))

              (define/private (lift-refactoring-available? pos)
                (cond
                  [(get-raw-refactor-info pos)
                   =>
                   (λ (refactor-info)
                     (match-define (cons span refactor-prop) refactor-info)
                     (and refactor-prop context-table
                          (find-surrounding-context context-table (syntax-info source pos span))))]
                  [else #f]))

              (define/private (swap-then-else-refactoring-available? pos)
                (cond
                  [(get-raw-refactor-info pos) => cdr]
                  [else #f]))

              (define/private (do-lift-if pos)
                (when context-table
                  (define refactor-info (get-raw-refactor-info pos))
                  (when refactor-info
                    (match-define (cons span refactor-prop) refactor-info)
                    (when refactor-prop
                      (match-define (list lang if-loc test-loc then-loc else-loc) refactor-prop)
                      (define loc (syntax-info source pos span))
                      (define outer-context
                        (find-surrounding-context context-table loc))
                      (when outer-context
                        (do-edit-if outer-context
                                    if-loc
                                    test-loc
                                    then-loc
                                    else-loc))))))

              (define/private (do-swap-then-else pos)
                (define refactor-info (get-raw-refactor-info pos))
                (when refactor-info
                  (match-define (cons span refactor-prop) refactor-info)
                  (when refactor-prop
                    (match-define (list lang if-loc test-loc then-loc else-loc) refactor-prop)
                    (define negate-id (get-negate-id lang))
                    (match-define (list test-start test-span) (shift test-loc))
                    (match-define (list then-start then-span) (shift then-loc))
                    (match-define (list else-start else-span) (shift else-loc))
                    (begin-edit-sequence)
                    (define then-text (copy-to-temp-text then-start (+ then-start then-span)))
                    (define else-text (copy-to-temp-text else-start (+ else-start else-span)))
                    (send this delete else-start (+ else-start else-span))
                    (send then-text move/copy-to-edit
                          this
                          0
                          then-span
                          else-start
                          #:try-to-move? #f)
                    (send this delete then-start (+ then-start then-span))
                    (send else-text move/copy-to-edit
                          this
                          0
                          else-span
                          then-start
                          #:try-to-move? #f)
                    ;; SHOULD FIX THE CONDITION HERE!!!
                    (end-edit-sequence)
                    (void))))

              (define/private (get-negate-id lang)
                "not")

              (define/private (do-edit-if ctx-loc if-loc test-loc then-loc else-loc)
                (match-define (list ctx-start ctx-span) (shift ctx-loc))
                (match-define (list if-start if-span) (shift if-loc))
                (match-define (list test-start test-span) (shift test-loc))
                (match-define (list then-start then-span) (shift then-loc))
                (match-define (list else-start else-span) (shift else-loc))

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

              (define/public (refactor-key-callback)
                (define pos-box (box #f))
                (send this get-position pos-box)
                (define pos (unbox pos-box))
                (do-lift-if pos))

              (super-new))))

        (define (add-refactor-keybindings keymap)
          (send keymap add-function
                "lift if expression"
                (λ (obj evt)
                  (send obj refactor-key-callback)))
          (send keymap map-function "c:x;l" "lift if expression"))

        (keymap:add-to-right-button-menu/before
         (let ([old (keymap:add-to-right-button-menu/before)])
           (λ (menu editor event)
             (old menu editor event)
             (define-values (pos text) (send editor get-pos/text event))
             (when (and pos (is-a? text text%))
               (send editor refactor-build-popup-menu menu pos text)))))

        (add-refactor-keybindings (drracket:rep:get-drs-bindings-keymap))
        (drracket:get/extend:extend-definitions-text make-refactor-text%)
        (drracket:module-language-tools:add-online-expansion-handler
         context-table.rkt
         'handle-expansion
         (λ (text expanded-info)
           (send text refactor-callback expanded-info)))))