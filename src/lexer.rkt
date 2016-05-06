#lang racket

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre)
         #;"maybe.rkt")

;; Parse the reduced-c grammar into s-expressions.

(define-tokens reducedc-datas
  (FLOAT INT ID STRING))

(define-empty-tokens reducedc-literals
  (CONST
   FUNCTION AUTO STRUCT
   BREAK
   CONTINUE
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

   FLOAT_T   ; types
   INT_T
   VOID_T
   BOOL_T

   TRUE
   FALSE
   RETURN

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
   LBRACK
   RBRACK
   LPAR
   RPAR
   LBRACE
   RBRACE
  ))

(define-lex-abbrevs
  ;; [letter     (:or (:/ "a" "z") (:/ #\A #\Z) )]
  ;; [digit      (:/ #\0 #\9)]
  ;; [identifier (:: letter (:* (:or letter digit #\_ #\?)))]

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
   [identifier (token-ID (string->symbol lexeme))]
   [string     (token-STRING lexeme)]

   [(:: "(" whitespace ")") (token-VOID)]

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
   ["void"     (token-VOID_T   )]
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
   ["["        (token-LBRACK   )]
   ["]"        (token-RBRACK   )]
   ["()"       (token-VOID     )]
   ["("        (token-LPAR     )]
   [")"        (token-RPAR     )]
   ["{"        (token-LBRACE   )]
   ["}"        (token-RBRACE   )]
   ))

(define (just x)
  (not (eq? x #f)))

(define-syntax (? x)
  (syntax-case
      [(_ x) #'(when (just x) x)]))

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
     [(global-decl global-decl) (cons $1 $2)])

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
      `(,$1 ,$2 ,$3 ,$4)
        ])

    (var-decl
     [(?static decorated-type ID ?array-list ?init SEMI)
      `(VAR ,(? $1) ,$2 ,$3 ,(? $4) ,(? $5))    ]
     [(?static AUTO ID ASSIGN expr SEMI)
      `(VAR ,(? $1) $2 $3 $5) ]
     [(?static struct-type ID ?array-list ?ctor-call SEMI)
      `(VAR ,(? $1) $2 $3 ,(? $4) ,(? $5))])

    (const-decl
     [(?static CONST basic-type ID ASSIGN const-expr SEMI)
      `(,$2 ,(? $1) ,$3 ,$4 ,$6)]
     [(?static CONST AUTO ID ASSIGN const-expr SEMI)
      ;; TODO: infer type, maybe in a separate pass?
      `(,$2 ,(? $1) ,$3 ,$4 ,$6)])

    (?static
     [(STATIC) $1]
     [()       #f])

    (struct-def-decl
     [(STRUCT ID LBRACE
              field-vars-list
              ctor-dtor-list
              ?field-funcs-list
              RBRACE SEMI)

      `($1 ,$2 )])

    (field-vars-list
     [(decorated-type ID ?array-list SEMI) ...]
     [(field-vars-list decorated-type ID ?array-list SEMI) ...])

    (ctor-dtor-list
     [() ...]
     [(ctor-dtor-decl ctor-dtor-list) ...])

    (ctor-dtor-decl
     ; ctor
     [(ID VOID
          LBRACE ?stmt-list RPAREN) ...]
     [(ID LPAR param-list RPAR
          LBRACE ?stmt-list RPAREN) ...]

     ; dtor
     [(TILDE ID VOID
             LBRACE ?stmt-list RBRACE) ...])

    (field-funcs-list
     [() ...]
     [(func-def field-funcs-list) ...])

    (func-def
     [(FUNCTION COLON ret-type ?ref ID
                VOID
                LBRACE ?stmt-list RBRACE) ...]

     [(FUNCTION COLON ret-type ?ref ID
                LPAR param-list RPAR
                LBRACE ?stmt-list RBRACE) ...])

    (func-decl
     [(EXTERN FUNCTION COLON ret-type ID
              VOID
              SEMI) ...]

     [(EXTERN FUNCTION COLON ret-type ID
              LPAR param-list RPAR
              SEMI) ...])

    ; omfg, can this be simplified
    (type
     [(decorated-type) ...]
     [(struct-type)...])

    (decorated-type
     [(basic-type ?pointer-list) ...]
     [(struct-type pointer-list) ...])

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
     [() ...]
     [(pointer-list)...])
    (pointer-list
     [(STAR pointer-list) ...]
     [() ...])

    (?array-list
     [() ...]
     [(LBRACK const-expr RBRACK ?array-list)...])

    (code-block
     [(LBRACE ?stmt-list RBRACE) ...])

    (?stmt-list
     [() ...]
     [(stmt stmt-list)...])

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
;;     [(EXIT VOID SEMI)              (list $1 $3)]
     [(EXIT LPAR expr RPAR SEMI)    (list $1 $3)]

     [(RETURN SEMI) $1]
     [(RETURN expr SEMI) (list $1 $2)]
     ;read
     [(CIN ISTREAM designator SEMI) ...]
     [(COUT OSTREAM write-pair-list SEMI) ...]
     [(NEW designator ?ctor-call SEMI) ...]
     [(DELETE designator SEMI) ...])

    (param-list
     [(param-decl) '()]
     [(param-decl COMMA param-list)....])
    (param-decl
     [(type ?ref ID ?array-list) ....])

    (?ref
     [() ...]
     [(AND) ...])
    (?init
     [(ASSIGN expr)...]
     [() ...])
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
     [(expr)...])

    (expr-list
     [(expr COMMA expr-list) ...]
     [(expr) ...])

    (expr
     [(designator ASSIGN expr) ...]
     [(expr0) ...])

    (expr0
     [(expr0 OROR expr1) ...]
     [(expr1) $1])

    (expr1
     [(expr1 ANDAND expr2) ...]
     [(expr2) $1])

    (expr2
     [(expr2 OR expr3) ...]
     [(expr3) $1])

    (expr3
     [(expr3 XOR expr4) ...]
     [(expr4) $1])

    (expr4
     [(expr4 AND expr5) ...]
     [(expr5) $1])

    (expr5
     [(expr5 comparison expr6) ...]
     [(expr6) $1])
    (comparison
     [(EQ) $1]
     [(NEQ) $1])
    (expr6
     [(expr6 relation expr7) ...]
     [(expr7) $1])
    (expr7
     [(expr7 add-op expr8)...]
     [(expr8) $1])
    (expr8
     [(expr8 mul-op designator)  ]
     [(designator) $1])

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
     [(PLUSPLUS) $1]
     [(MINUSMINUS) $1])
    (designator [(designator0) $1])
    (desingator0
     [(STAR desingator0) ...]
     [(AND desingator0) ...]
     [(BANG desingator0) ...]
     [(SIZEOF LPAR desingator0 RPAR) ...]
     [(SIZEOF LPAR type ?array-list RPAR) ...]
     [(LPAR decorated-type RPAR designator0) ...]
     [(inc-dec-op desingator0) ...]
     [(desingator1) ...])
    (designator1
     [(designator1 DOT ID) ...]
     [(designator1 LBRACK expr RBRACK) ...]
     [(designator1 ARROW ID) ...]
     [(designator1 inc-dec-op) ...]
     [(designator1 VOID) ...]
     [(designator1 LPAR expr-list RPAR) ...]
     [(designator2) ...])
    (desingator2
     [(PLUS desingator2) ...]
     [(MINUS desingator2) ...]
     [(LPAR expr RPAR) ...]
     [(INT) ...]
     [(FLOAT) ...]
     [(STR) ...]
     [(TRUE) ...]
     [(FALSE) ...]
     [(NULLPTR) ...]
     [(THIS) ...]
     [(COLONCOLON ID) ...]
     [(ID) ...])
    )))


(let ((test1 (open-input-file "test/m001.rc")))
 (reducedc-parser (λ () (reducedc-lexer test1))))
