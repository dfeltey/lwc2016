#lang racket/unit

(require parser-tools/lex
         syntax/readerr
         racket/port
         (prefix-in re: parser-tools/lex-sre)
         "tokens.rkt"
         "lexer-sig.rkt"
         "parser-sig.rkt"
         2d/readtable)

(import parser^)
(export lexer^)

(define (token->string t v)
  (if v
      (format "~a" v)
      (format "~a" t)))

;; syntax-coloring for use in get-info
(define (color-lexer in offset mode)
    (define tok (get-color-token in))
    (define (ret mode paren [eof? #f])
      (values (if eof?
                  eof
                  (token->string (position-token-token tok)
                                 (token-value (position-token-token tok))))
              mode 
              paren
              (position-offset (position-token-start-pos tok))
              (position-offset (position-token-end-pos tok))
              0 
              #f))
    (case (token-name (position-token-token tok))
      [(EOF) (ret 'eof #f #t)]
      [(O_PAREN) (ret 'parenthesis '|(|)]
      [(C_PAREN) (ret 'parenthesis '|)|)]
      [(O_BRACE) (ret 'parenthesis '|{|)]
      [(C_BRACE) (ret 'parenthesis '|}|)]
      [(O_BRACKET) (ret 'parenthesis '|[|)]
      [(C_BRACKET) (ret 'parenthesis '|]|)]
      [(IDENTIFIER) (ret 'symbol #f)]
      [(INTEGER_LIT) (ret 'constant #f)]
      [(White-Space) (ret 'white-space #f)]
      [(error) (ret 'error #f)]
      [else (ret 'other #f)]))

(define-lex-abbrevs
  (CR #\015)
  (LF #\012)
  (LineTerminator (re:or CR 
                         LF 
                         (re:: CR LF)))
  (InputCharacter (re:~ CR LF))
  
  (FF #\014)
  (TAB #\011)
  (NBSP #\uA0)
  (WhiteSpace (re:or #\space 
                     TAB
                     FF
                     LineTerminator
                     NBSP))

  (Comment (re:or TraditionalComment 
                  EndOfLineComment
                  DocumentationComment))
  (TraditionalComment (re:: "/*" NotStar CommentTail))
  (EndOfLineComment (re:: "//" (re:* InputCharacter)))
  (DocumentationComment (re:: "/**" CommentTailStar))
  (CommentTail (re:: (re:* (re:: (re:* NotStar) (re:+ "*") NotStarNotSlash))
                     (re:* NotStar)
                     (re:+ "*")
                     "/"))
  (CommentTailStar (re:: (re:* (re:: (re:* "*") NotStarNotSlash (re:* NotStar) "*"))
                         (re:* "*")
                         "/"))
  (NotStar (re:~ "*"))
  (NotStarNotSlash (re:~ "*" "/"))
  
  (SyntaxComment (re:or TraditionalCommentEOF
                        EndOfLineComment))
  (TraditionalCommentEOF (re:: "/*" CommentTailEOF))
  (CommentTailEOF (re:or (re:: (re:* (re:: (re:* NotStar) (re:+ "*") NotStarNotSlash))
                               (re:* NotStar)
                               (re:+ "*")
                               "/")
                         (re:: (re:* (re:: (re:* NotStar) (re:+ "*") NotStarNotSlash))
                               (re:* NotStar)
                               (re:* "*"))))  

  
  (Identifier (re:: JavaLetter (re:* JavaLetterOrDigit)))
  (JavaLetter (re:or (re:/ "AZ" "az") "_" "$"))
  (JavaLetterOrDigit (re:or JavaLetter (re:/ "09")))
  
  (KnownTypes (re:or "boolean" "byte" "int"))

  (Keyword (re:or "if"          "this"         "boolean"     "public"
                  "else"        "return"       "extends"     "int"
                  "static"      "void"         "class"       "super"
                  "while"       "const"        "for"        "new"
                  "switch"      "length"       "System.out.println"
                  "main"        "String"       "super"))
  
  (DecimalNumeral (re:or #\0
                         (re:: (re:/ "19") (re:* (re:/ "09")))))
  
  (Operator (re:or "&&"   "=="   "<"     "+"     "-"     "*" "%"      	"!"	"="   "||")))

;; Handle Comments
(define read-line-comment
  (lexer
   [(re:~ #\newline) (read-line-comment input-port)]
   [#\newline end-pos]
   [(eof) end-pos]
   [(special) (read-line-comment input-port)]
   [(special-comment) (read-line-comment input-port)]
   ))

(define read-block-comment
  (lexer
   ["*/" end-pos]
   [(eof) end-pos]
   [(re:or "*" "/" (complement (re:: any-string (re:or "*" "/") any-string))) (read-block-comment input-port)]
   [(special) (read-block-comment input-port)]
   [(special-comment) (read-block-comment input-port)]))

(define (make-get-token [color? #f])
  (define (next-token t) (if color? (get-color-token t) (get-token t)))
  (lexer-src-pos
   ("#lang" (token-LANG))
   ;; Special form for handling 2d syntax pass parse function to handle box contents
   ("#2" (let-values ([(line col pos) (port-next-location input-port)]
                      [(src) (file-path)]
                      [(next) (peek-char input-port)])
           (cond
             [(char=? next #\d)
              (token-2D (2d-readtable-dispatch-proc
                         #\2 input-port
                         src line col pos
                         (λ (input-port _1 _2)
                           (cond
                             [(eof-object? (peek-char input-port)) eof]
                             [else (parse input-port src 'box)]))
                         #f))]
             [color?
              (let loop ()
                 (define c (peek-char input-port))
                 (cond
                   [(eof-object? c)
                    (define-values (line col pos) (port-next-location input-port))
                    (return-without-pos
                     (make-position-token 'error
                                          start-pos
                                          (make-position pos line col)))]
                   [else
                    (read-char input-port)
                    (loop)]))]
             [else
              (raise-read-error
               (format "lexer2d: No match found in input starting with: ~a" (string-ref lexeme 0))
               (file-path)
               (position-line start-pos)
               (position-col start-pos)
               (position-offset start-pos)
               (- (position-offset end-pos) (position-offset start-pos)))])))
   
   (Operator (let ([l lexeme])
               (cond
                 [(string=? l "||") (token-OR)]
                 [else (string->symbol l)])))
   
   ("(" (token-O_PAREN))
   (")" (token-C_PAREN))
   ("{" (token-O_BRACE))
   ("}" (token-C_BRACE))
   ("[" (token-O_BRACKET))
   ("]" (token-C_BRACKET))
   (";" (token-SEMI_COLON))
   ("," (token-COMMA))
   ("." (token-PERIOD))
   
   ("true" (token-TRUE_LIT))
   ("false" (token-FALSE_LIT))
   ("break" (token-BREAK_LIT))
   
   (DecimalNumeral
    (token-INTEGER_LIT (string->number lexeme 10)))
   
   (Keyword (string->symbol lexeme))
   
   (Identifier (token-IDENTIFIER lexeme))
   
   ("//" (begin (read-line-comment input-port) (return-without-pos (next-token input-port))))
   ("/*" (begin (read-block-comment input-port) (return-without-pos (next-token input-port))))
   
   ((re:+ WhiteSpace) (if color?
                          (token-White-Space)
                          (return-without-pos (next-token input-port))))
   
   (#\032 'EOF)
   ((eof) 'EOF)
   [any-char
    (cond
      [color?
       (let loop ()
         (define c (peek-char input-port))
         (cond
           [(eof-object? c)
            (define-values (line col pos) (port-next-location input-port))
            (return-without-pos
             (make-position-token 'error
                                  start-pos
                                  (make-position pos line col)))]
           [else
            (read-char input-port)
            (loop)]))]
      [else
       (raise-read-error
        (format "lexer: No match found in input starting with: ~a" (string-ref lexeme 0))
        (file-path)
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos)))])]))



(define get-token (make-get-token))
(define get-color-token (make-get-token #t))
