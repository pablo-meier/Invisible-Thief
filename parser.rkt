#lang racket

(require rackunit)

(provide parse emit)

;; Proper error handling.

;; Ripped from the original whitespace VM's parser definitions. We first map
;; the three meaningful symbols (space, tab, linefeed) to the symbols A,B,C:

(define A #\space)
(define B #\tab)
(define C #\linefeed)

;; Then define some magical macros which generate the parse rules necessary
;; for parse and emit.

;; parse :: any/c * input-port -> [WS-Expr]
;;
;; Reads input from the parameter input-port and exports an AST for the 
;; whitespace program, which 'semantics.rkt' can execute.
(define (parse source-name input-port)
  (define (recursive accum)
    (cond
      [(eof-object? (peek-char input-port)) (reverse accum)]
      [(ignorable-char? input-port) (read-char input-port) (parse source-name input-port)]
      [else (let ([expr (ormap (λ (fn) (fn input-port)) parse-functions)])
              (if expr
                  (recursive (cons expr accum))
                  'invalid-expr))]))
  (recursive '()))

;; emit :: [WS-Expr] -> '()
;;
;; Writes a whitespaced-syntax representation of the input program to 
;; current-output-port.
(define (emit ws-prog)
  0)

(define emit-functions '())
(define parse-functions '())

(define-syntax (define-ws-parse-rules stx)
  (syntax-case stx ()
    [(_ parse-name prefix 
        (case-prefix (case-value case-suffix))
        (next-case-prefix (next-case-value next-case-suffix))
        ...)
     (let* ([checker (make-symbol-list-matcher (combine-symbols (list (syntax->datum #'prefix) 
                                                                      (syntax->datum #'case-prefix))) 
                                               stx)]
            [add-fun #`(set! parse-functions (cons (λ (input-port) (if (#,checker input-port)
                                                                       (handle-suffix 'case-suffix input-port)
                                                                       #f))
                                                   parse-functions))])
            #`(begin 
                #,add-fun
                (define-ws-parse-rules parse-name prefix
                  (next-case-prefix (next-case-value next-case-suffix))
                  ...)))]
    [(_ parse-name prefix) #'(void)]))

;; 'ABAB -> '(A B A B)
(define-for-syntax (split-symbol symb)
  (map (λ (x) (string->symbol (string x))) (string->list (symbol->string symb))))

;; '(A BB AA C) -> 'ABBAAC
(define-for-syntax (combine-symbols list-of-symbols)
  (string->symbol (apply string-append (map symbol->string list-of-symbols))))

;; make-symbol-list-matcher :: Symbol * Syntax -> #'(input-port -> bool)
;;
;; Given a compound symbol (e.g. 'A or 'CB) will create a function that reads from an input
;; port and returns true or false, if the chars read match the expanded form. Note that in
;; the #t case, we destructively read from the input-port, on the #f case, input-port is
;; unchanged.
(define-for-syntax (make-symbol-list-matcher compound-symbol stx)
  (let* ([list-of-symbols (split-symbol compound-symbol)]
         [los-as-syntax (datum->syntax stx (cons 'list list-of-symbols))])
    #`(λ (input-port)
        (letrec ([sequential (λ (offset)
                               (cond
                                 [(= offset #,(length list-of-symbols)) #t]
                                 [(eq? (peek-char input-port offset) (list-ref #,los-as-syntax offset))
                                  (sequential (add1 offset))]
                                 [else #f]))])
          (if (sequential 0)
              (repeat-read input-port #,(length list-of-symbols))
              #f)))))

(define (repeat-read input-port len)
  (cond
    [(= len 0) #t]
    [else (read-char input-port)
          (repeat-read input-port (sub1 len))]))



;; a macro to process these as LENSES: produces both an input function, and an output function
;; (from-whitespace, to-whitespace)

(define-ws-parse-rules parse-stack-instr A
  (A '(push NUMBER))
  (BA '(ref NUMBER))
  (BC '(slide NUMBER))
  (CA '(dup))
  (CB '(swap))
  (CC '(discard)))


(define-ws-parse-rules parse-control-instr C
  (AA '(label LABEL))
  (AB '(call LABEL))
  (AC '(jump LABEL))
  (BA '(ws-if zero LABEL))
  (BB '(ws-if negative LABEL))
  (BC '(return))
  (CC '(end)))

(define-ws-parse-rules arithmetic-instr BA
  (AA '(infix plus))
  (AB '(infix minus))
  (AC '(infix times))
  (BA '(infix divide))
  (BB '(infix modulo)))

(define-ws-parse-rules heap-access BB
  (A '(store))
  (B '(retrieve)))

(define-ws-parse-rules io-instr BC
  (AA '(output-char))
  (AB '(output-int))
  (BA '(read-char))
  (BB '(read-int)))


;                                                                 
;                                                                 
;                                                                 
;                                                                 
;   ;;                 ;;;                                        
;   ;;                   ;                                        
;   ;;                   ;                                        
;   ;; ;;      ;;;       ;     ;  ;;      ;;;     ;  ;;     ;;;   
;   ;;;  ;    ;   ;      ;     ;;;  ;    ;   ;    ;;;  ;   ;   ;  
;   ;;   ;   ;    ;;     ;     ;    ;;  ;    ;;   ;;       ;      
;   ;;   ;;  ;;;;;;;     ;     ;     ;  ;;;;;;;   ;        ;;;    
;   ;;   ;;  ;           ;     ;     ;  ;         ;          ;;;  
;   ;;   ;;  ;           ;     ;     ;  ;         ;            ;; 
;   ;;   ;;   ;   ;      ;     ;;   ;    ;   ;    ;       ;;   ;  
;   ;;   ;;    ;;;;   ;;;;;;   ; ;;;      ;;;;    ;        ;;;;   
;                              ;                                  
;                              ;                                  
;                              ;                                  

;; ignorable-char :: input-port -> bool
;;
;; Returns whether or not the next character to be read in should be ignored.
(define (ignorable-char? in)
  (let ([next-ch (peek-char in)])
    (not (member next-ch (list A B C)))))

;; parse-number :: input-port -> integer
;;
;; Reads a number from the input port, and returns the number. By whitespace
;; specification, this is a binary number with A = 0, B = 1, C = terminator.
(define (parse-number input-port)
  ;; turns '(1 0 0 1 1) -> 19
  (define (to-number lst)
    (letrec ([recur (λ (remaining accum pow)
                      (cond
                        [(null? remaining) accum]
                        [else (recur 
                                  (cdr remaining) (+ (* (car remaining) (arithmetic-shift 1 pow)) accum) (add1 pow))]))])
      (recur (reverse lst) 0 0)))
  (to-number (binary-sequence-helper input-port)))

(define (parse-string input-port)
  ;; turns '(0 1 1 0 1) -> '|01101|
  (define (to-label lst)
    (string->symbol (list->string (map (λ (x) (car (string->list (number->string x)))) lst))))
  (to-label (binary-sequence-helper input-port)))

(define (binary-sequence-helper input-port)
  (letrec ([recur (λ (accum)
                    (case (read-char input-port)
                      [(#\linefeed) (reverse accum)]
                      [(#\space) (recur (cons 0 accum))]
                      [(#\tab) (recur (cons 1 accum))]))])
    (recur '())))

;; handle-suffix : Suffix * Input-Port -> WsExpr
;;   where
;; Suffix = (instr) | (instr NUMBER) | (instr LABEL) | (instr mod)
;; WsExpr = A whitespace abstract syntax expression.
(define (handle-suffix suffix input-port)
  (define (upcase-symbol? s) 
    (and (symbol? s) (andmap char-upper-case? (string->list (symbol->string s)))))
  (define (remap-uppercase-symbol symb)
    (cond
      [(upcase-symbol? symb)
       (match symb
         ['NUMBER (parse-number input-port)]
         ['LABEL (parse-string input-port)])]
      [else symb]))
  (map remap-uppercase-symbol suffix))

;                                               
;                                               
;                                               
;                                               
;                                               
;     ;;                         ;;             
;     ;                          ;              
;   ;;;;;;     ;;;      ;;;    ;;;;;;     ;;;   
;     ;       ;   ;    ;   ;     ;       ;   ;  
;     ;      ;    ;;   ;         ;       ;      
;     ;      ;;;;;;;   ;;;       ;       ;;;    
;     ;      ;           ;;;     ;         ;;;  
;     ;      ;             ;;    ;           ;; 
;     ;;      ;   ;   ;;   ;     ;;     ;;   ;  
;      ;;;     ;;;;    ;;;;       ;;;    ;;;;   
;                                               
;                                               
;                                               

(check-equal?
 (parse-number (open-input-string (list->string (list B B A A B C))))
 25
 "Parse number test for 25")

(check-equal?
 (parse-number (open-input-string (list->string (list A C))))
 0
 "Parse number test for 0")

(check-equal?
 (parse-number (open-input-string (list->string (list B C))))
 1
 "Parse number test for 1")

(check-equal?
 (parse-string (open-input-string (list->string (list A B B A B C))))
 (string->symbol "01101")
 "Simple test")

(check-equal?
 (parse-string (open-input-string (list->string (list B C))))
 (string->symbol "1")
 "Length = 1 test")

(check-equal?
 (handle-suffix '(dup) (open-input-string (list->string (list B B B A C))))
 '(dup)
 "handle-suffix on unary operator")

(check-equal?
 (handle-suffix '(push NUMBER) (open-input-string (list->string (list B B B A C))))
 '(push 14)
 "handle-suffix on number insert")

(check-equal?
 (handle-suffix '(jump LABEL) (open-input-string (list->string (list B B B A C))))
 '(jump |1110|)
 "handle-suffix on label insert")

(check-equal?
 (handle-suffix '(infix plus) (open-input-string (list->string (list B B B A C))))
 '(infix plus)
 "handle-suffix on non-consuming argument")

(define-syntax (parse-test stx)
  (syntax-case stx ()
    [(_ description (id1 id2 ...) expected)
     #'(check-equal? 
        (parse #f (open-input-string (list->string (list id1 id2 ...))))
        expected
        description)]))
  
(parse-test
 "simple push test"
 (A A B A B C)
 '((push 5)))

(parse-test
 "ref test"
 (A B A B A C)
 '((ref 2)))
 
(parse-test
 "slide test"
 (A B C B A B A C)
 '((slide 10)))

(parse-test
 "dup test"
 (A C A)
 '((dup)))

(parse-test
 "swap test"
 (A C B)
 '((swap)))

(parse-test
 "discard test"
 (A C C)
 '((discard)))

(parse-test
 "call test"
 (C A B B B B A C)
 '((call |1110|)))

(parse-test
 "ws-if negative test"
 (C B B A A B C)
 '((ws-if negative |001|)))

(parse-test
 "end test"
 (C C C)
 '((end)))

(parse-test
 "multiple commands"
 (A A B A B C 
  A A B A B C 
  B A A A
  C C C)
 '((push 5)
   (push 5)
   (infix plus)
   (end)))