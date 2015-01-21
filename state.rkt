#lang racket

;; <program>   ::=  <exp> | <exp> <program>
;; <block>     ::=  '{'  { <exp> [';'] } <exp>  '}' | <exp>
;; <exp>       ::=  <number>
;;             ::=  <boolean>
;;             ::=  <string>
;;             ::=  <var>
;;             ::=  (<varlist>) -> <block>         
;;             ::=  <funcname> '(' <explist> ')'
;;             ::=  let <varlist> = <explist>
;;             ::=  let <var> <funcname>(<varlist>) = <block>
;;             ::=  fn <funcname>(<varlist>) <block>
;;             ::=  if <exp> <block> {elseif <block>} [else <block>]
;;             ::=  <exp> <binop> <exp>
;;             ::=  <unop> <exp>
;; <newline>   ::=  \n
;; <funcname>  ::=  (\w[\d]?)+
;; <var>       ::=  (\w[\d]?)+
;; <varlist>   ::=  <var> {',' <var>}
;; <explist>   ::=  {<exp> ','} <exp>
;; <number>    ::=  [-0-9\.]+
;; <boolean>   ::=  false | false
;; <string>    ::=  ((?:\\.|[^\\:])*)(?::|$)
;; <binop>     ::=  '+'' | '-' | '*' | '/' | '^' | '%' | '..' | '<' | '<=' | '>' | '>=' | '==' | '!=' | 'and' | 'or' | '*' | '&'
;; <unop>      ::=  '-' | 'not' | '#'



(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)

(require (for-syntax syntax/stx))
(require "helper.rkt")


;; define exp structs
(define-syntax (def-exp stx)
  (syntax-case stx ()
    [(_ parent-exp child-exp ...)
     #`(begin 
         (struct parent-exp () #:transparent)
         #,@(for/list ([exp (syntax->list #'(child-exp ...))])
             #`(struct #,(stx-car exp) parent-exp #,(stx-car (stx-cdr exp)) 
                 #:transparent)))]))


(def-exp program-exp ; -> (struct program-exp () #:transparent)
  [const-exp  (num)] ; -> (struct const-exp program-exp (num) #:transparent)
  [diff-exp  (exp1 exp2)]
  [zero?-exp  (exp1)]
  [equal?-exp  (exp1 exp2)]
  [greater?-exp  (exp1 exp2)]
  [less?-exp  (exp1 exp2)]
  [if-exp  (exp1 exp2 exp3)]
  [var-exp  (var)]
  [let-exp  (var exp1 body)]
  [add-exp  (exp1 exp2)]
  [minus-exp  (exp)]
  [mul-exp  (exp1 exp2)]
  [quotient-exp   (exp1 exp2)]
  [begin-exp (exp1 exp2)]
  ;proc interpreter
  [lambda-exp  (argument body)]
  [app-exp  (procedure argument)]
  ;recursive declarations interpreter
  [letrec-exp (proc var proc-body let-body)]
  ;nameless interpreter for static environments
  [nameless-var-exp (num)]
  [nameless-let-exp (exp1 body)]
  [nameless-lambda-exp (body)])

(def-exp value-exp
  [text (value)]
  [numeric (value)]
  [closure (argument body environment)]
  [reference (address)]
  [pair (left right)]
  [nclosure (body environment)])

 
(define-tokens value-tokens (NUM ID))
(define-empty-tokens op-tokens 
  (= LET EQ ENDEQ IN LPAR RPAR COMMA SUB EOF ZERO IF 
     MINUS ADD MUL QUOTIENT EQUAL GREATER LESS PROC LETPROC LETREC
     BEGIN END))


;; Lexer for the LET Language, Chapter 3
;let-lexer : input-port? -> (or/c symbol? number?)
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
   ["letproc" 'LETPROC]
   ["letrec" 'LETREC]
   ["in" 'IN]
   ["proc" 'PROC]
   ["zero?" 'ZERO]
   ["equal?" 'EQUAL]
   ["greater?" 'GREATER]
   ["less?" 'LESS]
   ["if" 'IF]
   [#\: 'ENDEQ]
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
    
    (exp [(NUM) (const-exp $1)]
         [(ID) (var-exp $1)]
         [(LET exp EQ exp ENDEQ IN exp) 
          (let-exp $2 $4 $7)]
         [(IF exp exp exp)
          (if-exp $2 $3 $4)]
         ;;PROCEDURES creation handler
         [(PROC LPAR exp RPAR exp) 
          (lambda-exp $3 $5)]
         [(LETPROC exp LPAR exp RPAR exp ENDEQ IN exp)
          (let-exp $2 ;procedure name
                   (lambda-exp $4 $6) ;procedure body
                   $9)] ; let expression body
         [(LETREC ID LPAR ID RPAR EQ exp ENDEQ IN exp)
          (letrec-exp 
           (var-exp $2) 
           (var-exp $4) 
           $7 $10)]
         ;;SUB MINUS ADD MUL QUOTIENT
         [(SUB LPAR exp COMMA exp RPAR)
          (diff-exp $3 $5)]
         [(ADD LPAR exp COMMA exp RPAR)
          (add-exp $3 $5)]    
         [(MUL LPAR exp COMMA exp RPAR)
          (mul-exp $3 $5)]
         [(QUOTIENT LPAR exp COMMA exp RPAR)
          (quotient-exp $3 $5)]         
         [(MINUS LPAR exp RPAR)
          (minus-exp $3)]
         [(ZERO LPAR exp RPAR)
          (zero?-exp $3)]
         [(EQUAL LPAR exp COMMA exp RPAR)
          (equal?-exp $3 $5)]
         [(GREATER LPAR exp COMMA exp RPAR)
          (greater?-exp $3 $5)]
         [(LESS LPAR exp COMMA exp RPAR)
          (less?-exp $3 $5)]
         [(exp LPAR exp RPAR)
          (app-exp $1 $3)]
         ))))



;; nameless interpreter for static environment


;;environment management
;empty-senv :  -> (listof symbol?)
(define (empty-senv)
  '())

;extend-senv : symbol?  (listof symbol?) -> (listof symbol?)
(define (extend-senv var senv)
  (cons var senv))

;apply-senv : (listof symbol?) symbol? -> number?
(define (apply-senv senv var)
  (list-index senv var))


;;translate whole programs
;; translate-program : program-exp? -> program-exp?
(define (translate-program program)
  (translate-exp program (empty-senv)))




;;get the AST
;; string? -> program-exp?
(define (ast source)
  (define source-code (open-input-string source))
  (let-parser (lambda () (let-lexer source-code))))



(define test-prg "let x = 37: in proc(y) let z = -(x, y): in -(x,z)")

;(run "proc(x) -(x,11)")
(module+ test
  (require rackunit)
  (check-pred lambda-exp? (ast "proc(x) -(x,11)"))
  (check-pred let-exp?  (ast "let f = proc(x) -(x, 11): in f(f(77))"))
  (check-equal? (run "let z = 2: in if less?(z,1) 1 0") 0)
  (check-equal? (run "let f = proc(x) -(x, 11): in f(12)") 1)
  (check-equal? (run "let z = 2: in if less?(z,1) 1 0") 0)
  (check-equal? (run  "letproc sumone(x) +(x,1): in sumone(10)") 11)
  (check-equal? 50
                (run "
                     let sum = proc (y) proc (x) +(x, y): 
                      in let curry = sum(20): 
                        in curry(30)"))
  (check-equal? -100 
                (run "
                      let x = 200: 
                       in let f = proc (z) -(z,x): % f = z - 200
                        in let x = 100: % x = 100
                         in let g = proc (z) -(z,x):  %g = z - 100
                          in -(f(1),
 g(1)) % (1 - 200) - (1 - 100)")))
