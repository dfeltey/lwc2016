#lang racket

(require "lexer.rkt"
         "ast.rkt"
         "parameter.rkt")           

(require parser-tools/yacc
         (except-in parser-tools/lex input-port)
         syntax/readerr)

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
     [(MainClass) (list $1)]
     [(MainClass ClassDeclarations) (cons $1 (reverse $2))])
    
    (MainClass
     [(class Identifier O_BRACE MainMethod C_BRACE)
      (make-class-def (make-header $2 null null)
                      $4
                      #f
                      #f)])
    
    (MainMethod
     [(public static void main O_PAREN String O_BRACKET C_BRACKET Identifier C_PAREN O_BRACE Statement C_BRACE)
      (make-method (list 'public 'static)
                   (make-type-spec 'void 0)
                   (make-id 'main)
                   null
                   (make-block null $12 #f))])
    
    (ClassDeclarations
     [(ClassDeclaration) (list $1)]
     [(ClassDeclarations ClassDeclaration) (cons $2 $1)])
    
    (ClassDeclaration
     [(class Identifier ClassBody)
      (make-class-def (make-header $2 null null) #f (car $3) (cadr $3))]
     [(class Identifier extends Identifier ClassBody)
      (make-class-def (make-header $2 null (list $4)) #f (car $5) (cadr $5))])
    
    (ClassBody
     [(O_BRACE VariableDeclarations MethodDeclarations C_BRACE) (list (reverse $2) (reverse $3))]
     [(O_BRACE VariableDeclarations C_BRACE) (list (reverse $2) null)]
     [(O_BRACE MethodDeclarations C_BRACE) (list null (reverse $2))]
     [(O_BRACE C_BRACE) (list null null)])
    
    (MethodDeclarations
     [(MethodDeclaration) (list $1)]
     [(MethodDeclarations MethodDeclaration) (cons $2 $1)])
    
    (MethodDeclaration
     [(MethodHeader MethodBody) (make-method (method-modifiers $1)
                                             (method-type $1)
                                             (method-id $1)
                                             (method-parms $1)
                                             $2)])
    
    (MethodHeader
     [(public Type MethodDeclarator) (construct-method-header 'public $2 $3)])
    
    (MethodDeclarator
     [(IDENTIFIER O_PAREN FormalParameterList C_PAREN) (list $1 (reverse $3) 0)]
     [(IDENTIFIER O_PAREN C_PAREN) (list $1 null 0)])
    
    (MethodBody
     [(O_BRACE VariableDeclarations BlockStatements return Expression SEMI_COLON C_BRACE) (make-block (reverse $2) (reverse $3) $5)]
     [(O_BRACE VariableDeclarations return Expression C_BRACE) (make-block (reverse $2) null $4)]
     [(O_BRACE BlockStatements return Expression C_BRACE) (make-block null (reverse $2) $4)]
     [(O_BRACE return Expression C_BRACE) (make-block null null $3)])
    
    (FormalParameterList
     [(FormalParameter) (list $1)]
     [(FormalParameterList COMMA FormalParameter) (cons $3 $1)])
    
    (FormalParameter
     [(Type Identifier) (make-var-decl $2 null $1)])
    
    (VariableDeclarations
     [(VariableDeclaration) (list $1)]
     [(VariableDeclarations VariableDeclaration) (cons $2 $1)])
    
    (VariableDeclaration
     [(Type Identifier SEMI_COLON) (make-var-decl $2 null $1)])
    
    (Type
     [(int O_BRACKET C_BRACKET) (make-type-spec 'int 1)]
     [(boolean) (make-type-spec 'boolean 0)]
     [(int) (make-type-spec 'int 0)]
     [(Identifier) (make-type-spec 'id 0)])
    
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
     [(O_BRACE BlockStatements C_BRACE) (make-block null (reverse $2))]
     [(O_BRACE C_BRACE) (make-block null null)])
    
    (IfThenElseStatement
     [(if O_PAREN Expression C_PAREN Statement else Statement)
      (make-ifS $3 $5 $7)])
    
    (WhileStatement
     [(while O_PAREN Expression C_PAREN Statement)
      (make-while $3 $5)])
    
    (Println
     [(System.out.println O_PAREN Expression C_PAREN SEMI_COLON)
      (make-println $3)])
    
    (Assignment
     [(LeftHandSide AssignmentOperator Expression SEMI_COLON)
      (make-assignment $1 $2 $3)])      
    
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
      (make-bin-op'&& $1 $3)])
    
    (RelationalExpression
     [(AdditiveExpression) $1]
     [(RelationalExpression < AdditiveExpression)
      (make-bin-op '< $1 $3)])
    
    (AdditiveExpression
     [(MultiplicativeExpression) $1]
     [(AdditiveExpression + MultiplicativeExpression)
      (make-bin-op '+ $1 $3)]
     [(AdditiveExpression - MultiplicativeExpression)
      (make-bin-op '- $1 $3)])
    
    (MultiplicativeExpression
     [(UnaryExpression) $1]
     [(MultiplicativeExpression * UnaryExpression)
      (make-bin-op '* $1 $3)])
    
    (UnaryExpression
     [(Primary) $1]
     [(! UnaryExpression) (make-unary '! $2)])
    
    (Primary
     [(Literal) $1]
     [(Identifier) $1]
     [(this) (make-special-name "this")]
     [(ArrayAccess) $1]
     [(O_PAREN Expression C_PAREN) $2]
     [(ClassInstanceCreationExpression) $1]
     [(FieldAccess) $1]
     [(MethodInvocation) $1]
     [(ArrayCreationExpression) $1])
    
    (Literal
     [(INTEGER_LIT) (make-literal 'int $1)]
     [(TRUE_LIT) (make-literal 'boolean #t)]
     [(FALSE_LIT) (make-literal 'boolean #f)])
    
    (Identifier
     [(IDENTIFIER) (make-id $1)])
    
    (ArrayAccess 
     [(Primary O_BRACKET Expression C_BRACKET)
      (make-array-access $1 $3)])
    
    (ClassInstanceCreationExpression
     [(new Identifier O_PAREN C_PAREN)
      (make-class-alloc $2 null)])
    
    (FieldAccess
     [(Primary PERIOD length) 
      (make-field-access $1 (make-id 'length))])
    
    (MethodInvocation
     [(Primary PERIOD Identifier O_PAREN ArgumentList C_PAREN)
      (make-call $1 (make-id $3) (reverse $5))]
     [(Primary PERIOD Identifier O_PAREN C_PAREN)
      (make-call $1 (make-id $3) null)])
    
    (ArgumentList
     [(Expression) (list $1)]
     [(ArgumentList COMMA Expression) (cons $3 $1)])
    
    (ArrayCreationExpression
     [(new int DimExpr) (make-array-alloc 'int $3)])
    
    (DimExpr
     [(O_BRACKET Expression C_BRACKET) $2]))))