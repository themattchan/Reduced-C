#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         "types.rkt")

;; Lex and Parse the reduced-c grammar into s-expressions.

;; if these are already the labels for the actual datums
;; then we can discard the additional empty type token
(define-tokens reducedc-datas
  (FLOAT INT ID STRING))

(define-empty-tokens reducedc-literals
  (; keywords
   CONST
   FUNCTION
   AUTO
   STRUCT
   BREAK
   CONTINUE
   RETURN
   DO
   FOR
   WHILE
   SWITCH
   CASE
   DEFAULT
   IF
   ELSE
   OUT
   INOUT
   IN

   ; types
   FLOAT_T
   INT_T
   BOOL_T
   VOID                                 ; VOID == UNIT == NULL

   TRUE
   FALSE

   ; symbols and operators
   SEMI
   PLUS
   MINUS
   STAR
   SLASH
   PERCENT
   LT
   LE
   GT
   GE
   ASSIGN
   EQ
   NEQ
   ANDAND
   AND
   OROR
   OR
   BANG
   DOT
   COMMA
   TILDE
   LBRACK
   RBRACK
   LPAR
   RPAR
   LBRACE
   RBRACE
  ))

(define-lex-abbrevs
  [integer    (:+ numeric)]
  [float      (:: (:+ numeric) #\. (:* numeric))]
  [identifier (:: alphabetic (:* (:or alphabetic numeric "_-")))]
  [string     (:: #\" (:* (:~ #\")) #\")]
  )

;; input-port -> symbol
;; generates successive symbols while consuming input port
(define reducedc-lexer
  (lexer
   ; skip whitespace
   [whitespace (reducedc-lexer input-port)]

   ; tokens with things in them
   [integer    (token-INT   (string->number lexeme))]
   [float      (token-FLOAT (string->number lexeme))]
   [identifier (token-ID    (string->symbol lexeme))]
   [string     (token-STRING lexeme)]

   [(:: "(" (:* whitespace) ")") (token-VOID)]

    ; constants
   [(eof)       '()             ]
   ["const"    (token-CONST    )]
   ["function" (token-FUNCTION )]
   ["structdef"(token-STRUCT   )]
   ["auto"     (token-AUTO     )]
   ["break"    (token-BREAK    )]
   ["continue" (token-CONTINUE )]
   ["do"       (token-DO       )]
   ["for"      (token-FOR      )]
   ["while"    (token-WHILE    )]
   ["switch"   (token-SWITCH   )]
   ["case"     (token-CASE     )]
   ["default"  (token-DEFAULT  )]
   ["if"       (token-IF       )]
   ["else"     (token-ELSE     )]
   ["out"      (token-OUT      )]
   ["inout"    (token-INOUT    )]
   ["in"       (token-IN       )]
   ["float"    (token-FLOAT_T  )]
   ["int"      (token-INT_T    )]
   ["void"     (token-VOID     )]
   ["bool"     (token-BOOL_T   )]
   ["true"     (token-TRUE     )]
   ["false"    (token-FALSE    )]
   ["return"   (token-RETURN   )]
   [";"        (token-SEMI     )]
   ["+"        (token-PLUS     )]
   ["-"        (token-MINUS    )]
   ["*"        (token-STAR     )]
   ["/"        (token-SLASH    )]
   ["%"        (token-PERCENT  )]
   ["<"        (token-LT       )]
   ["<="       (token-LE       )]
   [">"        (token-GT       )]
   [">="       (token-GE       )]
   ["="        (token-ASSIGN   )]
   ["=="       (token-EQ       )]
   ["!="       (token-NEQ      )]
   ["&&"       (token-ANDAND   )]
   ["&"        (token-AND      )]
   ["||"       (token-OROR     )]
   ["|"        (token-OR       )]
   ["!"        (token-BANG     )]
   ["."        (token-DOT      )]
   [","        (token-COMMA    )]
   ["~"        (token-TILDE    )]
   ["["        (token-LBRACK   )]
   ["]"        (token-RBRACK   )]
   ["()"       (token-VOID     )]
   ["("        (token-LPAR     )]
   [")"        (token-RPAR     )]
   ["{"        (token-LBRACE   )]
   ["}"        (token-RBRACE   )]
   ))

(define-syntax (? x)
  (syntax-case
      [(_ x) #'(when (not (eq? x #f)) x)]))

; TODO: make a list of all the symbols in the target grammar

;;  (() -> symbol) -> sexpr
(define reducedc-parser
  (parser
   (tokens reducedc-datas reducedc-literals)
   (start program)
   (end EOF)
   (error void)
   (grammar
    (program
     [() '()]
     [(global-decl) $1]
     [(global-decl program) (cons $1 $2)])

    ;; (args [(exp)            (b o (list $1) 1 1)]
    ;;       [(exp COMMA args) (b o (cons $1 $3) 1 3)]
    ;;       [() '()])
    (global-decl
     [(extern-decl)     $1]
     [(var-decl)        $1]
     [(const-decl)      $1]
     [(struct-def-decl) $1]
     [(func-decl)       $1]
     [(func-def)        $1])

    (extern-decl
     [(EXTERN decorated-basic-type ID ?array-list SEMI)
      (rc/extern-var $3 $2 $4) ])

    ;; TODO: check if type exists
    (var-decl
     [(?static decorated-type ID ?array-list ?init SEMI)
      (rc/var $3 $1 $2 $4 $5) ]
     [(?static AUTO ID ASSIGN expr SEMI)
      ;; TODO: type inference
      (rc/var $3 $1 $2 $2 $5)]
     [(?static struct-type ID ?array-list ?ctor-call SEMI)
      ;; TODO: value should be value of ctor-call, [[$5]]
      ;; also struct-type should have been previously defined
      (rc/var $3 $1 $2 $4 $5)])

    (const-decl
     [(?static CONST basic-type ID ASSIGN const-expr SEMI)
      (rc/val $4 $1 $3 $6)]
     [(?static CONST AUTO ID ASSIGN const-expr SEMI)
      ;; TODO: infer type, maybe in a separate pass?
      (rc/val $4 $1 $3 $6)])

    (?static
     [(STATIC) #t]
     [()       #f])

    (struct-def-decl
     [(STRUCT ID LBRACE
              field-vars-list
              ctor-dtor-list
              ?field-funcs-list
              RBRACE SEMI)

      (rc/struct $2 $4 $5 $6) ])

    (field-vars-list
     [(field-var) $1]
     [(field-var field-vars-list) (cons $1 $2)])

    ;; what is this and why is it different from the vars above
    (field-var
     [(decorated-type ID ?array-list SEMI)
      (rc/var $2 $1 $3)])


    (ctor-dtor-list
     [() '()]
     [(ctor-dtor-decl ctor-dtor-list) (cons $1 $2)])

    (ctor-dtor-decl
     ; ctor
     [(ID VOID
          LBRACE ?stmt-list RPAREN)
      (rc/ctor $1 $2 $4)]
     [(ID LPAR param-list RPAR
          LBRACE ?stmt-list RPAREN)
      (rc/ctor $1 $2 $4)]

     ; dtor
     [(TILDE ID VOID
             LBRACE ?stmt-list RBRACE)
      (rc/dtor $2 $5)])

    (field-funcs-list
     [() '()]
     [(func-def field-funcs-list) (cons $1 $2)])

    (func-def
     [(FUNCTION COLON ret-type ?ref ID
                VOID
                LBRACE ?stmt-list RBRACE)

      (rc/func $5 $3 $4 $6 $8)]

     [(FUNCTION COLON ret-type ?ref ID
                LPAR param-list RPAR
                LBRACE ?stmt-list RBRACE)
      (rc/func $5 $3 $4 $6 $8)])

    (func-decl
     [(EXTERN FUNCTION COLON ret-type ID
              VOID
              SEMI)
      (rc/extern-func $5 $4 $6)]

     [(EXTERN FUNCTION COLON ret-type ID
              LPAR param-list RPAR
              SEMI)
      (rc/extern-func $5 $4 $6)])

    ;; TODO TODO TODO
    (type
     [(basic-type ?pointer-list) (rc/type $1 $2)]
     [(struct-type ?pointer-list) (rc/type $1 $2)])

    (basic-type
     [(INT_T)   $1]
     [(FLOAT_T) $1]
     [(BOOL_T)  $1])

    (struct-type
     [(ID) $1])

    (ret-type
     [(type) $1]
     [(VOID) $1])

    (?pointer-list
     [() 0]
     [(STAR pointer-list) (add1 $2)])

    (?array-list
     [() '()]
     [(LBRACK const-expr RBRACK ?array-list) (cons $2 $4)])

    (code-block
     [(LBRACE ?stmt-list RBRACE) (rc/block $2)])

    (?stmt-list
     [() '()]
     [(stmt stmt-list) (cons $1 $2)])

    (stmt
     [(var-decl)                    $1]
     [(const-decl)                  $1]
     [(code-block)                  $1]
     [(expr SEMI)                   $1]
     [(if-stmt)                     $1]
     [(while-stmt)                  $1]
     [(foreach-stmt)                $1]
     [(BREAK SEMI)                  $1]
     [(CONTINUE SEMI)               $1]
     [(EXIT LPAR expr RPAR SEMI)    (list $1 $3)]

     [(RETURN SEMI) (list $1 'VOID)]
     [(RETURN expr SEMI) (list $1 $2)]

     ; IO
     [(CIN ISTREAM designator SEMI) ...]
     [(COUT OSTREAM write-pair-list SEMI) ...]

     ; object creation
     [(NEW designator ?ctor-call SEMI) ...]
     [(DELETE designator SEMI) ...])

    (param-list
     [(param-decl) (list $1)]
     [(param-decl COMMA param-list) (cons $1 $3)])
    (param-decl  ; these are vars... what is this ref business
     [(type ?ref ID ?array-list) ....])

    (?ref
     [() #f]
     [(AND) 'REF])

    (?init
     [(ASSIGN expr) $2]
     [() 'VOID])
    (?ctor-call
     [(COLON VOID)....]
     [(COLON LPAR expr-list RPAR)....]
     [() ...])
    (if-stmt
     [(IF expr code-block ?else)...])
    (?else
     [() ...]
     [(ELSE code-block)...])
    (while-stmt
     [(WHILE expr code-block)...])
    (foreach-stmt
     [(FOREACH LPAR type ?ref ID COLON expr RPAR
               code-block)...])

    (write-pair-list
     [(write-pair) ...]
     [(write-pair OSTREAM write-pair-list)...])
    (write-pair
     [(expr) ...]
     [(ENDL) ...])

    (const-expr
     [(expr) $1])

    (expr-list
     [(expr COMMA expr-list) (cons $1 $3)]
     [(expr) (list $1)])

    (expr
     [(designator ASSIGN expr) `(ASSIGN ,$1 ,$3)]
     [(expr0) $1])
    (expr0
     [(expr0 OROR expr1) `(OR ,$1 ,$3)]
     [(expr1) $1])
    (expr1
     [(expr1 ANDAND expr2) `(AND ,$1 ,$3)]
     [(expr2) $1])
    (expr2
     [(expr2 OR expr3) `(BITWISE-OR ,$1 ,$3)]
     [(expr3) $1])
    (expr3
     [(expr3 XOR expr4) `(BITWISE-XOR ,$1 ,$3)]
     [(expr4) $1])
    (expr4
     [(expr4 AND expr5) `(BITWISE-AND ,$1 ,$3)]
     [(expr5) $1])
    (expr5
     [(expr5 comparison expr6) `(,$2 ,$1 ,$3)]
     [(expr6) $1])
    (expr6
     [(expr6 relation expr7) `(,$2 ,$1 ,$3)]
     [(expr7) $1])
    (expr7
     [(expr7 add-op expr8) `(,$2 ,$1 ,$3)]
     [(expr8) $1])
    (expr8
     [(expr8 mul-op designator) `(,$2 ,$1 ,$3)]
     [(designator) $1])

    (comparison
     [(EQ)  $1]
     [(NEQ) $1])
    (relation
     [(LT) $1]
     [(GT) $1]
     [(LE) $1]
     [(GE) $1])
    (add-op
     [(PLUS) $1]
     [(MINUS) $1])
    (mul-op
     [(STAR) 'TIMES]
     [(SLASH) 'DIVIDE]
     [(PERCENT) 'MOD])
    (inc-dec-op
     [(PLUSPLUS) 'INCREMENT]
     [(MINUSMINUS) 'DECREMENT])

    ;; TODO: need some forms for these "functions"
    ;; should generate somewhat normalized code

    ;; TODO: translate all pointer arithmetic to PTR-INC and PTR-DEC

    (designator [(designator0) $1])

    (desingator0
     [(STAR desingator0) `(DEREF ,$2)]
     [(AND desingator0) `(REF ,$2)]
     [(BANG desingator0) `(NOT ,$2)]
     ; SIZEOF is overloaded
     [(SIZEOF LPAR desingator0 RPAR) `(,$1 ,$3)]
     [(SIZEOF LPAR type ?array-list RPAR) `(,$1 ,$3 ,$4)]
     [(LPAR decorated-type RPAR designator0) `(CAST ,$2 ,$4)]
     [(inc-dec-op desingator0) `(,(string->symbol (~a 'PRE- $1)) ,$2)]
     [(desingator1) $1])

    (designator1
     [(designator1 DOT ID) `(MEMBER ,$1 ,$3)]
     [(designator1 LBRACK expr RBRACK) `(DEREF (PTR-INC ,$1 ,$3))] ; array access
     [(designator1 ARROW ID) `(MEMBER (DEREF ,$1) ,$3)]
     [(designator1 inc-dec-op) `(,(string->symbol (~a 'POST- $1)) ,$2)]
     [(designator1 VOID) `(,$1 ,$2)]
     [(designator1 LPAR expr-list RPAR) `(,$1 ,@$3)]
     [(designator2) $1])

    (designator2
     [(PLUS designator2) ,$2]
     [(MINUS designator2) `(NEGATE ,$2)]
     [(LPAR expr RPAR) $2] ;; is this right
     ;; literals
     [(INT) ...]
     [(FLOAT) ...]
     [(STR) ...]
     [(TRUE) ...]
     [(FALSE) ...]
     [(NULLPTR) ...]
     [(THIS) $1]
     [(COLONCOLON ID) ...]
     [(ID) ...])
    )))


(let ((test1 (open-input-file "test/m001.rc")))
 (reducedc-parser (λ () (reducedc-lexer test1))))
