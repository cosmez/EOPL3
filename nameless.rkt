#lang racket
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
  ;proc interpreter
  [lambda-exp  (argument body)]
  [app-exp  (procedure argument)]
  ;recursive declarations interpreter
  [letrec-exp (proc var proc-body let-body)]
  ;nameless interpreter for static environments
  [nameless-var-exp (num)]
  [nameless-let-exp (exp1 body)]
  [nameless-lambda-exp (body)])

;; this is a value type
(struct closure (argument body environment) #:transparent #:mutable) 

(struct nclosure (body environment) #:transparent #:mutable)



(define-tokens value-tokens (NUM ID))
(define-empty-tokens op-tokens 
  (= LET EQ ENDEQ IN LPAR RPAR COMMA SUB EOF ZERO IF 
     MINUS ADD MUL QUOTIENT EQUAL GREATER LESS PROC LETPROC LETREC))


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


;;translate exp
(define (translate-exp exp senv)
  (match exp
    [(const-exp num) exp]
    [(var-exp var) (nameless-var-exp  (apply-senv senv var))]
    ;;Arithmetic Operations
    [(diff-exp exp1 exp2) ;=>
     (diff-exp (translate-exp exp1 senv) (translate-exp exp2 senv))]
    [(add-exp exp1 exp2) ;=>
     (add-exp (translate-exp exp1 senv) (translate-exp exp2 senv))]
    [(mul-exp exp1 exp2) ;=>
     (mul-exp (translate-exp exp1 senv) (translate-exp exp2 senv))]
    [(quotient-exp exp1 exp2) ;=>
     (quotient-exp (translate-exp exp1 senv) (translate-exp exp2 senv))]
    [(minus-exp exp) ;=>
     (minus-exp (translate-exp exp senv))]
    ;;Boolean Ops
    [(zero?-exp exp) ;=>
     (zero?-exp (translate-exp exp senv))]
    [(equal?-exp exp1 exp2) 
     ;=>
     (equal?-exp (translate-exp exp1 senv) (translate-exp exp2 senv))]
    [(greater?-exp exp1 exp2);=>
     (greater?-exp (translate-exp exp1 senv) (translate-exp exp2 senv))]
    [(less?-exp exp1 exp2);=>
     (less?-exp (translate-exp exp1 senv) (translate-exp exp2 senv))]
    ;; Branching
    [(if-exp cond-exp if-exp else-exp);=>
     (if-exp (translate-exp cond-exp senv)
             (translate-exp if-exp senv)
             (translate-exp else-exp senv))]
    ;; proc language extension
    ;; application/closure expression
    [(app-exp app-name app-body);=>
     (app-exp (translate-exp app-name senv) (translate-exp app-body senv))]
    ;; lambda expression
    ;; a lambda expression evaluates to a closure
    [(lambda-exp argument body);=>
     (define argument-name (var-exp-var argument))
     (nameless-lambda-exp 
      (translate-exp 
       body (extend-senv argument-name senv)))]
    ;; recursive let
    [(letrec-exp proc var proc-body let-body) ;=>
     exp]
    ;; normal let
    [(let-exp var-exp binding-exp body-exp) ;=>
     (define variable-name (var-exp-var var-exp))
     (nameless-let-exp 
      (translate-exp binding-exp senv)
      (translate-exp body-exp (extend-senv variable-name senv)))]))



;;nameless valued environment
;empty-env : -> (listof (or/c closure? number?))
(define (empty-nenv)
  '())

;extend-nenv (or/c closure? number?)) (listof (or/c closure? number?)) -> (listof (or/c closure? number?))
(define (extend-nenv value nenv)
  (cons value nenv))

;apply-nenv (listof (or/c closure? number?)) number? -> (or/c closure? number?)
(define (apply-nenv nenv address)
  (list-ref nenv address))

;ninterp program-exp? (lisrof (or/c closure? number?)) -> (or/c closure? number?)
(define (ninterp exp nenv)
  (match exp
    [(const-exp num) num]
    [(nameless-var-exp ref) (apply-nenv nenv ref)]
    ;;Arithmetic Operations
    [(diff-exp exp1 exp2) (- (ninterp exp1 nenv) (ninterp exp2 nenv))]
    [(add-exp exp1 exp2) (+ (ninterp exp1 nenv) (ninterp exp2 nenv))]
    [(mul-exp exp1 exp2) (* (ninterp exp1 nenv) (ninterp exp2 nenv))]
    [(quotient-exp exp1 exp2) (quotient (ninterp exp1 nenv) (ninterp exp2 nenv))]
    [(minus-exp exp) (* (ninterp exp nenv) -1)]
    ;;Boolean Ops
    [(zero?-exp exp) (= (ninterp exp nenv) 0)]
    [(equal?-exp exp1 exp2) (= (ninterp exp1 nenv) (ninterp exp2 nenv))]
    [(greater?-exp exp1 exp2) (> (ninterp exp1 nenv) (ninterp exp2 nenv))]
    [(less?-exp exp1 exp2) (< (ninterp exp1 nenv)(ninterp exp2 nenv))]
    ;; Branching
    [(if-exp cond-exp if-exp else-exp)
     (define cond-result (ninterp cond-exp nenv))
     (if cond-result (ninterp if-exp nenv) (ninterp else-exp nenv))]
    ;; proc language extension
    ;; application/closure expression
    [(app-exp procedure argument)
     (match-define 
      (nclosure body-exp clos-nenv) ;=>
      (ninterp procedure nenv))
      (define argument-value (ninterp argument nenv))
      (define body-nenv (extend-nenv body-value clos-nenv))
      ;; TODO: is this correct?
      (ninterp body-exp body-nenv)
     ]
    ;; lambda expression
    ;; a lambda expression evaluates to a closure
    [(nameless-lambda-exp body)
     (nclosure body nenv)]
    [(nameless-let-exp binding-exp body-exp);=>
     (define binding-value (ninterp binding-exp nenv))
     (define new-nenv (extend-nenv binding-value nenv))
     (ninterp body-exp new-nenv)]))




(define test-prg "let x = 37: in proc(y) let z = -(x, y): in -(x,z)")
