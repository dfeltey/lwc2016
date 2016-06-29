#lang racket

(require "lexer.rkt"
         "ast.rkt"
         "parameter.rkt")           

(require parser-tools/yacc
         (except-in parser-tools/lex input-port)
         syntax/readerr
         racket/syntax)

(provide parse)

(define (lex-port port filename)
    (port-count-lines! port)
    (file-path filename)
    (letrec ((getter
              (lambda (acc)
                (let ((cur-tok (get-token port)))
                  (if (eq? 'EOF (position-token-token cur-tok))
                      (cons cur-tok acc)
                      (getter (cons cur-tok acc)))))))
      (reverse (getter null))))

(define (getter token-list)
    (lambda ()
      (begin0 (car token-list)
              (unless (null? (cdr token-list))
                (set! token-list (cdr token-list))))))

(define (parse is filename)
    (let* ((lexed (lex-port is filename))
           (my-get (getter lexed)))
      (parse-mini-java my-get)))

(define-syntax (build-src stx)
    (syntax-case stx ()
      ((_ end)
       (syntax (build-src 1 end)))
      ((_ start end)
       (with-syntax ((start-pos (datum->syntax 
                                 (syntax end)
                                 (string->symbol 
                                  (format "$~a-start-pos"
                                          (syntax->datum (syntax start))))))
                     (end-pos (datum->syntax 
                               (syntax end)
                               (string->symbol 
                                (format "$~a-end-pos"
                                        (syntax->datum (syntax end)))))))
         (syntax
          (make-src (position-line start-pos)
                    (position-col start-pos)
                    (+ (position-offset start-pos) (interactions-offset))
                    (- (position-offset end-pos)
                       (position-offset start-pos))
                    (file-path)
                    ))))))

(define (construct-method-header mods ret-type declarator)
  (make-method mods 
               (make-type-spec (type-spec-name ret-type)
                               (+ (type-spec-dim ret-type) (caddr declarator)))
               (car declarator)
               (cadr declarator)
               #f))

(define parse-mini-java
  (parser
   (start Program)
   (tokens java-vals Keywords ExtraKeywords Separators EmptyLiterals Operators)
   (error (lambda (tok-ok name val start-pos end-pos)
            (raise-read-error (format "Parse error near <~a:~a>" name val)
                              (file-path)
                              (position-line start-pos)
                              (position-col start-pos)
                              (+ (position-offset start-pos) (interactions-offset))
                              (- (position-offset end-pos)
                                 (position-offset start-pos)))))
   
   (end EOF)
   (src-pos)
   
   (grammar
    
    (Program
     [(MainClass) (list $1)
                  #;(datum->syntax #f
                     `(,$1)
                     (src->list (build-src 1)))]
     [(MainClass ClassDeclarations) (cons $1 (reverse $2)) 
      #;(datum->syntax #f
                     `(,$1 ,@(reverse $2))
                     (src->list (build-src 2)))])

    (MainClass
     [(class Identifier O_BRACE MainMethod C_BRACE)
      (datum->syntax #f
                     `(class ,$2 { ,@$4 })
                     (src->list (build-src 5)))])

    (MainMethod
     [(public static void main O_PAREN String O_BRACKET C_BRACKET Identifier C_PAREN O_BRACE Statement C_BRACE)
      (datum->syntax #f
                     `(public static void main (String[] ,$9) {,$12})
                     (src->list (build-src 13)))])
    
    (ClassDeclarations
     [(ClassDeclaration) (list $1)]
     [(ClassDeclarations ClassDeclaration) (cons $2 $1)])

    (ClassDeclaration
     [(class Identifier ClassBody)
      (datum->syntax #f
                     `(class ,$2 { ,@$3 })
                     (src->list (build-src 3)))]
     [(class Identifier extends Identifier ClassBody)
      (datum->syntax #f
                     `(class ,$2 extends ,$4{ ,$5 })
                     (src->list (build-src 5)))])
    
    (ClassBody
     [(O_BRACE VariableDeclarations MethodDeclarations C_BRACE)
      (datum->syntax #f
                     `(,@$2 ,@$3)
                     (src->list (build-src 4)))]
     [(O_BRACE VariableDeclarations C_BRACE)
      (datum->syntax #f
                     `(,@$2)
                     (src->list (build-src 3)))]
     [(O_BRACE MethodDeclarations C_BRACE)
      (datum->syntax #f
                     `(,@$2)
                     (src->list (build-src 3)))]
     [(O_BRACE C_BRACE) 
      (datum->syntax #f
                     `()
                     (src->list (build-src 2)))])
    
    (MethodDeclarations
     [(MethodDeclaration) (list $1)]
     [(MethodDeclarations MethodDeclaration) (cons $2 $1)])

    (MethodDeclaration
     [(MethodHeader MethodBody) (datum->syntax #f
                                               `(public ,@$1 ,$2)
                                               (src->list (build-src 2)))])

    (MethodHeader
     [(public Type MethodDeclarator)
      `(,@$2 ,@$3)])

    (MethodDeclarator
     [(Identifier O_PAREN FormalParameterList C_PAREN)
      `(,$1 ,@$3)]
     [(Identifier O_PAREN C_PAREN)
      `(,$1 ())])
    
    (MethodBody
     [(O_BRACE VariableDeclarations BlockStatements return Expression SEMI_COLON C_BRACE)
      (datum->syntax #f
                     `( ,@$2 ,@$3 return ,$5)
                     (src->list (build-src 7)))]
     [(O_BRACE VariableDeclarations return Expression SEMI_COLON C_BRACE)
      (datum->syntax #f
                     `( ,@$2 return ,$4)
                     (src->list (build-src 6)))]
     [(O_BRACE BlockStatements return Expression SEMI_COLON C_BRACE)
      (datum->syntax #f
                     `( ,@$2 return ,$4)
                     (src->list (build-src 6)))]
     [(O_BRACE return Expression SEMI_COLON C_BRACE)
      (datum->syntax #f
                     `( return ,$3)
                     (src->list (build-src 5)))])
    
    (FormalParameterList
     [(FormalParameter) (list $1)]
     [(FormalParameterList COMMA FormalParameter) (cons $3 $1)])
    
    (FormalParameter
     [(Type Identifier)
      (datum->syntax #f
                     `(,@$1 ,$2)
                     (src->list (build-src 2)))])
    
    (VariableDeclarations
     [(VariableDeclaration) (list $1)]
     [(VariableDeclarations VariableDeclaration) (cons $2 $1)])
    
    (VariableDeclaration
     [(Type Identifier SEMI_COLON)
      (datum->syntax #f
                     `(,@$1 ,$2)
                     (src->list (build-src 2)))])
    
    (Type
     [(int O_BRACKET C_BRACKET) '(int ())]
     [(boolean) '(boolean)]
     [(int) '(int)]
     [(Identifier) `(,$1)])
    
    (Statement
     [(Block) $1]
     [(IfThenElseStatement) $1]
     [(WhileStatement) $1]
     [(Println) $1]
     [(Assignment) $1])
    
    (BlockStatements
     [(Statement) (list $1)]
     [(BlockStatements Statement) (cons $2 $1)])
    
    (Block
     [(O_BRACE BlockStatements C_BRACE)
      (datum->syntax #f
                     `(,@$2)
                     (src->list (build-src 3)))]
     [(O_BRACE C_BRACE)
      (datum->syntax #f
                     `()
                     (src->list (build-src 2)))])
    
    (IfThenElseStatement
     [(if O_PAREN Expression C_PAREN Statement else Statement)
      (datum->syntax #f
                     `(if (,$3) ,$5 else ,$7)
                     (src->list (build-src 7)))])
    
    (WhileStatement
     [(while O_PAREN Expression C_PAREN Statement)
      (datum->syntax #f
                     `(while (,$3) ,$5)
                     (src->list (build-src 5)))])
    
    (Println
     [(System.out.println O_PAREN Expression C_PAREN SEMI_COLON)
      (datum->syntax #f
                     `(System.out.println (,$3))
                     (src->list (build-src 5)))])
    
    (Assignment
     [(LeftHandSide AssignmentOperator Expression SEMI_COLON)
      (datum->syntax #f
                     `(,$1 = ,$3)
                     (src->list (build-src 4)))])      
    
    (LeftHandSide
     [(Identifier) $1]
     [(ArrayAccess) $1])
    
    (AssignmentOperator
     [(=) '=])    
    
    (Expression
     [(ConditionalAndExpression) $1])
    
    (ConditionalAndExpression
     [(RelationalExpression) $1]
     [(ConditionalAndExpression && RelationalExpression)
      (datum->syntax #f
                     `(,$1 && ,$3)
                     (src->list (build-src 3)))])
    
    (RelationalExpression
     [(AdditiveExpression) $1]
     [(RelationalExpression < AdditiveExpression)
      (datum->syntax #f
                     `(,$1 < ,$3)
                     (src->list (build-src 3)))])
    
    (AdditiveExpression
     [(MultiplicativeExpression) $1]
     [(AdditiveExpression + MultiplicativeExpression)
      (datum->syntax #f
                     `(,$1 + ,$3)
                     (src->list (build-src 3)))]
     [(AdditiveExpression - MultiplicativeExpression)
      (datum->syntax #f
                     `(,$1 - ,$3)
                     (src->list (build-src 3)))])
    
    (MultiplicativeExpression
     [(UnaryExpression) $1]
     [(MultiplicativeExpression * UnaryExpression)
      (datum->syntax #f
                     `(,$1 * ,$3)
                     (src->list (build-src 3)))])
    
    (UnaryExpression
     [(Primary) $1]
     [(! UnaryExpression) (datum->syntax #f
                                         `(! ,$2)
                                         (src->list (build-src 2)))])
    
    (Primary
     [(Literal) $1]
     [(Identifier) $1]
     [(this) (datum->syntax #f
                            'this
                            (src->list (build-src 1)))]
     [(ArrayAccess) $1]
     [(O_PAREN Expression C_PAREN) $2]
     [(ClassInstanceCreationExpression) $1]
     [(FieldAccess) $1]
     [(MethodInvocation) $1]
     [(ArrayCreationExpression) $1])
    
    (Literal
     [(INTEGER_LIT) (datum->syntax #f
                                  $1
                                  (src->list (build-src 1)))]
     [(TRUE_LIT) (datum->syntax #f
                                  #t
                                  (src->list (build-src 1)))]
     [(FALSE_LIT) (datum->syntax #f
                                  #f
                                  (src->list (build-src 1)))])

    (Identifier
     [(IDENTIFIER) (datum->syntax #f
                                  (string->symbol $1)
                                  (src->list (build-src 1)))])
    
    (ArrayAccess 
     [(Primary O_BRACKET Expression C_BRACKET)
      (datum->syntax #f
                     `(,$1 [ ,$3])
                     (src->list (build-src 4)))])
    
    (ClassInstanceCreationExpression
     [(new Identifier O_PAREN C_PAREN)
      (datum->syntax #f
                     `(new ,$2 ())
                     (src->list (build-src 4)))])
    
    (FieldAccess
     [(Primary PERIOD length) 
      (datum->syntax #f
                     `(,$1 length)
                     (src->list (build-src 3)))])
    
    (MethodInvocation
     [(Primary PERIOD Identifier O_PAREN ArgumentList C_PAREN)
      (datum->syntax #f
                     `(,$1 ,$3(,@$5))
                     (src->list (build-src 6)))]
     [(Primary PERIOD Identifier O_PAREN C_PAREN)
      (datum->syntax #f
                     `(,$1 ,$3())
                     (src->list (build-src 5)))])
    
    (ArgumentList
     [(Expression) (list $1)]
     [(ArgumentList COMMA Expression) (cons $3 $1)])
    
    (ArrayCreationExpression
     [(new int DimExpr)
      (datum->syntax #f
                     `(new int[,$3])
                     (src->list (build-src 3)))])
    
    (DimExpr
     [(O_BRACKET Expression C_BRACKET) $2]))))