#lang racket

(require drracket/tool
         framework
         racket/gui
         racket/runtime-path
         "context-table.rkt"
         "syntax-info.rkt"
         data/interval-map)

(provide tool@)

(define-runtime-path context-table.rkt "context-table.rkt")

(define tool@
  (unit (import drracket:tool^)
        (export drracket:tool-exports^)

        (define (phase1) (void))
        (define (phase2) (void))

        (define make-refactor-text%
          (mixin (racket:text<%>) ()
            (inherit begin-edit-sequence end-edit-sequence
                     insert delete)

            (define refactor-info #f)

            (define/private (update-refactor-info! [info #f])
              (set! refactor-info (and info (make-interval-map info))))

            (define/private (invalidate-refactor-info!)
              (update-refactor-info!))

            (define/public (refactor-callback expanded-info)
              (update-refactor-info! expanded-info))

            (define/public (refactor-build-popup-menu menu pos text)
              (define refactor-menu
                (make-object menu%
                  "Refactor"
                  menu))
              (send refactor-menu enable #f)
              (when (get-refactor-info pos)
                (new menu-item%
                     [label "Swap then/else branches"]
                     [parent refactor-menu]
                     [callback (λ (item evt) (do-swap-then-else pos))])
                (send refactor-menu enable #t)))

            (define/private (get-refactor-info pos)
              (and refactor-info
                   (interval-map-ref refactor-info pos #f)))

            (define/private (do-swap-then-else pos)
              (define refactor-info (get-refactor-info pos))
              (when refactor-info
                (match-define (list lang if-loc test-loc then-loc else-loc) refactor-info)
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
                (do-negate-test-edit test-start test-span lang)
                (invalidate-refactor-info!)
                (end-edit-sequence)
                (void)))

            (define/private (get-negate-id lang)
              (case lang
                [(racket) "not"]
                [else "!"]))

            (define/private (do-negate-test-edit start span lang)
              (begin-edit-sequence)
              (define negate-id (get-negate-id lang))
              (send this insert ")" (+ start span))
              (case lang
                [(racket)
                 (send this insert (string-append "(" negate-id " ") start)]
                [(mini-java)
                 (send this insert (string-append negate-id " " "(") start)])
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
              (do-swap-then-else pos))

            (super-new)))

        (define (add-refactor-keybindings keymap)
          (send keymap add-function
                "swap if branches"
                (λ (obj evt)
                  (send obj refactor-key-callback)))
          (send keymap map-function "c:r;s" "swap if branches"))

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