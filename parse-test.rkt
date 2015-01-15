#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(define-tokens value-tokens (NUM ID))
(define-empty-tokens op-tokens 
  (= LET EQ ENDLET IN LPAR RPAR COMMA SUB EOF ZERO IF 
     MINUS ADD MUL QUOTIENT EQUAL GREATER LESS PROC LETPROC))

;; Lexer for the LET Language, Chapter 3
(define let-lexer
  (lexer
   [#\- 'SUB]
   [#\+ 'ADD]
   [#\* 'MUL]
   [#\/ 'QUOTIENT]
   [#\- 'SUB]
   ["minus" 'MINUS]
   [#\( 'LPAR]
   [#\) 'RPAR]
   [#\, 'COMMA]
   [#\= 'EQ]
   ["let" 'LET]
   ["in" 'IN]
   ["proc" 'PROC]
   ["letproc" 'LETPROC]
   ["zero?" 'ZERO]
   ["equal?" 'EQUAL]
   ["greater?" 'GREATER]
   ["less?" 'LESS]
   ["if" 'IF]
   [#\: 'ENDLET]
   ;; TODO: theres a bug with numbers in identifiers
   ;; for example: sum1
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z))) 
    ; =>
    (token-ID (string->symbol lexeme))]
   [(:+ (:or (char-range #\0 #\9) #\.)) 
    ; =>
    (token-NUM (string->number lexeme))]
   [#\% (let-comment-lexer input-port)]
   [(union #\space #\newline) (let-lexer input-port)]
   [(eof) 'EOF]))

;;lex comment secitons % 
(define let-comment-lexer
  (lexer
   [#\newline (let-lexer input-port)]
   [any-char (let-comment-lexer input-port)]
   [(eof) 'EOF]))


(define (format-lets lets let-body)
  (displayln "")
  (displayln lets)
  (displayln "")
  (match lets
    ['() let-body]
    [(list (list 'let var body) rest ...) 
     `(let ,var ,body ,(format-lets rest let-body))]))


;;let token parser
(define let-parser
  (parser
   
   (start start)
   (end EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))
   
   (precs 
    (left SUB))
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    (let-list
     [(ID EQ exp) (list (list 'let (list 'var $1) $3))]
     [(ID EQ exp COMMA let-list) 
      (append 
       (list 
        (list 'let (list 'var $1) $3) ) 
       $5)])
    (exp [(NUM) (list 'cons $1)]
         [(ID) (list 'var $1)]
         [(LET let-list ENDLET IN exp)
          (format-lets $2 $5)]
         [(IF exp exp exp)
          (list 'if $2 $3 $4)]
         ;;PROCEDURES creation handler
         [(PROC LPAR exp RPAR exp) 
          (list 'proc $3 $5)]
         [(LETPROC exp LPAR exp RPAR exp ENDLET IN exp)
          (list 'letproc $2 $4 $6 $9)]
         ;;SUB MINUS ADD MUL QUOTIENT
         [(SUB LPAR exp COMMA exp RPAR)
          (list 'sub $3 $5)]
         [(ADD LPAR exp COMMA exp RPAR)
          (list 'add $3 $5)]
         [(MUL LPAR exp COMMA exp RPAR)
          (list 'mul $3 $5)]
         [(QUOTIENT LPAR exp COMMA exp RPAR)
          (list 'quotient $3 $5)]         
         [(MINUS LPAR exp RPAR)
          (list 'minus $3)]
         [(ZERO LPAR exp RPAR)
          (list 'zero $3)]
         [(EQUAL LPAR exp COMMA exp RPAR)
          (list 'equal $3 $5)]
         [(GREATER LPAR exp COMMA exp RPAR)
          (list 'greater $3 $5)]
         [(LESS LPAR exp COMMA exp RPAR)
          (list 'less $3 $5)]
         [(exp LPAR exp RPAR)
          (list 'apply $1 $3)]))))


;;get the tokens
;; string? -> list?
(define (tokenize source)
  (define (join-tokens input)
    (define token (let-lexer input))
    (if (equal? token 'EOF) '() (cons token (join-tokens input))))
  (define source-code (open-input-string source))
  (join-tokens source-code))

;;get the AST
;; string? -> program-exp?
(define (ast source)
  (define source-code (open-input-string source))
  (let-parser (lambda () (let-lexer source-code))))

