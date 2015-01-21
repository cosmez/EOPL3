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
  [nameless-lambda-exp (body)]
  ;state management
  [begin-exp (exp1 exp2)]
  [display-exp (exp)])

;; this is a value type
(struct closure (argument body environment) #:transparent #:mutable) 

(define-tokens value-tokens (NUM ID))
(define-empty-tokens op-tokens 
  (= LET EQ ENDEQ IN LPAR RPAR COMMA SUB EOF ZERO IF 
     MINUS ADD MUL QUOTIENT EQUAL GREATER LESS PROC 
     LETPROC LETREC BEGIN END DISPLAY SEMICOLON))

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
   ["begin" 'BEGIN]
   [";" 'SEMICOLON]
   ["end" 'END]
   ["display" 'DISPLAY]
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
         ;;STATE
         [(BEGIN exp SEMICOLON exp END)
          (begin-exp $2 $4)]
         [(DISPLAY exp)
          (display-exp $2)]
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



;;Global Store
;;Environment
;; empty-store : -> hash?
(define (empty-store)
  (hash))

;; extend-store : hash? number? number? -> hash?
(define (extend-store store address value)
  (hash-set store address value))

;; apply-env : hash? symbol? -> (or/c? closure number?)
(define (apply-store store var)
  (hash-ref store var))


;;Environment
;; empty-env : -> hash?
(define (empty-env)
  (hash))

;; extend-env : hash? symbol? number? -> hash?
(define (extend-env env var value)
  (hash-set env var value))

;; apply-env : hash? symbol? -> (or/c? closure number?)
(define (apply-env env var)
  (hash-ref env var))

;; join-env : hash? hash? -> hash?
(define (join-env env extend-env)
  (for/fold ([tmp env]) ([(key value) (in-hash extend-env)]) 
    (hash-set tmp key value)))

;;get the AST
;; string? -> program-exp?
(define (ast source)
  (define source-code (open-input-string source))
  (let-parser (lambda () (let-lexer source-code))))


;; run the AST
;; string? -> (or/c closure? number?)
(define (run source)
  (interp (ast source) (empty-env)))



;; interp the ast with the environment
;; program-exp? ->  (or/c closure? number?)
(define (interp exp env)
  (match exp
    [(const-exp num) num]
    [(var-exp var) (apply-env env var)]
    ;;Arithmetic Operations
    [(diff-exp exp1 exp2) (- (interp exp1 env) (interp exp2 env))]
    [(add-exp exp1 exp2) (+ (interp exp1 env) (interp exp2 env))]
    [(mul-exp exp1 exp2) (* (interp exp1 env) (interp exp2 env))]
    [(quotient-exp exp1 exp2) (quotient (interp exp1 env) (interp exp2 env))]
    [(minus-exp exp) (* (interp exp env) -1)]
    ;;Boolean Ops
    [(zero?-exp exp) (= (interp exp env) 0)]
    [(equal?-exp exp1 exp2) (= (interp exp1 env) (interp exp2 env))]
    [(greater?-exp exp1 exp2) (> (interp exp1 env) (interp exp2 env))]
    [(less?-exp exp1 exp2) (< (interp exp1 env)(interp exp2 env))]
    ;; Branching
    [(if-exp cond-exp if-exp else-exp)
     (define cond-result (interp cond-exp env))
     (if cond-result (interp if-exp env) (interp else-exp env))]
    ;; proc language extension
    ;; application/closure expression
    [(app-exp app-name app-body)
     (match-define ;get the closure from the environment
       (closure clos-argument clos-body clos-environment)
       (apply-env env (var-exp-var app-name)))
     ; this is the application exp, we need the result to feed the closure
     (define app-body-value (interp app-body env))
     ;the closure env is the current env plus the clos inner env
     (define new-closure-env
       (join-env 
        ;the close argument gets replaced by the application result
        (extend-env env (var-exp-var clos-argument) app-body-value) 
        clos-environment))
     (interp clos-body new-closure-env)]
    ;; lambda expression
    ;; a lambda expression evaluates to a closure
    [(lambda-exp body argument)
     (closure body argument env)]
    ;; recursive let
    [(letrec-exp proc var proc-body let-body)
     (define proc-name (var-exp-var proc))
     (define proc-argument (var-exp-var var))
     (define tmp-closure-env env)
     ;;closure with a tmp environment
     (define new-closure (closure var proc-body tmp-closure-env))
     (define let-environment (extend-env env proc-name new-closure))
     ;;to be able to see the binding in the closure body
     ;;we need to mutate the environment to include its own binding
     (set-closure-environment! new-closure let-environment)
     (interp let-body let-environment)]
    ;;state management
    [(display-exp exp1)
     (displayln (interp  exp1 env))]
    [(begin-exp exp1 exp2)
     (interp exp1 env)
     (interp exp2 env)]
    [(let-exp var-exp binding-exp body-exp)
     (define variable-name (var-exp-var var-exp))
     (define variable-value (interp binding-exp env)) 
     (interp body-exp (extend-env env variable-name variable-value))]))










(run "begin display 2; 10 end")

;letrec fact(x) = if zero?(x) 1 *(x,fact(-x(x,1))): in fact(5)





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
                        in curry(30)")
  (check-equal? -100 
                (run "
                      let x = 200: 
                       in let f = proc (z) -(z,x): % f = z - 200
                        in let x = 100: % x = 100
                         in let g = proc (z) -(z,x):  %g = z - 100
                          in -(f(1),
 g(1)) % (1 - 200) - (1 - 100)")))
  (check-equal? 120
                (run "
                     letrec fact(x) = 
                             if zero?(x) 1 *(x,fact(-(x,1))): 
                      in fact(5)")))
