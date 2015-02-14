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
(struct reference (address) #:transparent #:mutable)
(struct tuple (first rest) #:transparent #:mutable)

(define let-value? (or/c closure? number? reference? tuple?))

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
          (app-exp $1 $3)]))))


;;Environment
;; empty-env : -> hash?
(define (empty-env)
  (hash))

;; extend-env : hash? symbol? number? -> hash?
(define (extend-env env var value)
  (hash-set env var value))

;; apply-env : hash? symbol? -> let-value?
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
;; string? -> let-value?
(define (run source)
  (interp (ast source) (empty-env) end-cont))

;; apply-cont : lambda? let-value? 
(define (apply-cont cont value)
  (cont value))

;;end-cont : let-value? void?
(define (end-cont v)
  (printf "End of Computation: ~a\n" v))

;;zero-cont : continuation? -> let-value?
(define (zero-cont k)
  (lambda (val)
    (apply-cont k (= val 0))))

;;let-cont symbol? program-exp? environment? continuation? -> let-value?
(define (let-exp-cont variable-name body-exp env k)
  (lambda (val)
    (interp body-exp 
            (extend-env env variable-name val)
            k)))



(define (if-cont if-exp else-exp env k)
  (lambda (cond-val) 
    (if cond-val 
        (interp if-exp env k) 
        (interp else-exp env k))))


(define (app-cont app-name env k)
  (lambda (app-body-result)
    (match-define ;get the closure from the environment
     (closure clos-argument clos-body clos-environment)
     (apply-env env (var-exp-var app-name)))
    (define app-body-value app-body-result)
    (define new-closure-env
      (join-env 
       ;the close- argument gets replaced by the application result
       (extend-env env (var-exp-var clos-argument) app-body-value) 
       clos-environment))
    (interp clos-body new-closure-env k)))

;; interp the ast with the environment
;; program-exp? ->  let-value?
(define (interp exp env k)
  (printf "~a =/= ~a\n" exp env)
  (match exp
    [(const-exp num) (apply-cont k num)]
    [(var-exp var) (apply-cont k (apply-env env var))]
    ;; lambda expression
    ;; a lambda expression evaluates to a closure
    [(lambda-exp body argument)
     (apply-cont k (closure body argument env))]
    [(zero?-exp exp) (interp exp env (zero-cont k))]

    ;; let with continuation
    [(let-exp var-exp binding-exp body-exp)
     (define variable-name (var-exp-var var-exp))
     (define let-cont (let-exp-cont variable-name body-exp env k))
     (interp binding-exp env let-cont)]    
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
     ;;the proc part doesnt get executed, thats why
     ;;we dont need to execute that and continue with the body
     (interp let-body let-environment k)]
    ;; application expression
    [(app-exp app-name app-body)
     (define app-exp-cont (app-cont app-name env k))
     (interp app-body env app-exp-cont)]
    
    ;; if with continuations
    [(if-exp cond-exp if-exp else-exp)
     (define if-exp-cont (if-cont if-exp else-exp env k))
     (interp cond-exp env if-exp-cont)]
    
    ;;Arithmetic Operations
    [(diff-exp exp1 exp2) (- (interp exp1 env) (interp exp2 env))]
    [(add-exp exp1 exp2) (+ (interp exp1 env) (interp exp2 env))]
    [(mul-exp exp1 exp2) (* (interp exp1 env) (interp exp2 env))]
    [(quotient-exp exp1 exp2) (quotient (interp exp1 env) (interp exp2 env))]
    [(minus-exp exp) (* (interp exp env) -1)]
    ;;Boolean Ops
    
    [(equal?-exp exp1 exp2) (= (interp exp1 env) (interp exp2 env))]
    [(greater?-exp exp1 exp2) (> (interp exp1 env) (interp exp2 env))]
    [(less?-exp exp1 exp2) (< (interp exp1 env)(interp exp2 env))]
    ;; proc language extension
    ;; application/closure expression
        
    ;;state management
    [(display-exp exp1)
     (displayln (interp  exp1 env))]
    [(begin-exp exp1 exp2)
     (interp exp1 env)
     (interp exp2 env)]
    ))



(run "letrec some(x) = zero?(x): in some(0)")


(module+ test
  (require rackunit)
  (check-pred lambda-exp? (ast "proc(x) -(x,11)"))
  (check-pred let-exp?  (ast "let f = proc(x) -(x, 11): in f(f(77))"))
  (check-equal?  0 (run "let z = 2: in if less?(z,1) 1 0"))
  (check-equal?  1 (run "let f = proc(x) -(x, 11): in f(12)"))
  (check-equal?  0 (run "let z = 2: in if less?(z,1) 1 0") )
  (check-equal? 11 (run  "letproc sumone(x) +(x,1): in sumone(10)"))
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
