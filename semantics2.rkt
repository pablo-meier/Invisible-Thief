#lang racket

(require rackunit)
(provide interp-program)


;                    
;                    
;                    
;                    
;   ;     ;  ;     ; 
;   ;     ;  ;     ; 
;   ;;   ;;  ;;   ;; 
;    ;   ;   ;;  ;;; 
;    ;   ;   ; ; ; ; 
;    ;; ;;   ; ;;  ; 
;     ; ;    ;  ;  ; 
;     ; ;    ;     ; 
;     ;;;    ;     ; 
;      ;     ;     ; 
;      ;     ;     ; 
;                    
;                    
;                    

;; Represents a Whitespace VM
(define-struct state (program value-stack call-stack memory program-counter escape))

;; Parameter representing the state the operations mutate.
(define-syntax (define-whitespace-operations stx)
  (syntax-case stx ()
    [(_ (destructure ...) 
        [(id1 id2 ...)
         body1 ...]
        [(id3 id4 ...)
         body2 ...] ...)
     #'(begin
         (set! operations
               (cons (λ (current-state expr)
                       (match expr
                         [(list 'id1 id2 ...)
                          (match current-state
                            [(struct state (destructure ...))
                             (increment-pc (begin body1 ...))])]
                         [else #f]))
                     operations))
         ;; Recursion...
         (define-whitespace-operations (destructure ...)
           [(id3 id4 ...)
            body2 ...] ...))]
    [(_ (destructure ...))
     #'(void)]))

;; Filled in by macros: a list of functions mapping to Whitespace operations
;; that take an s-expression. If they match, the operation is invoked.
(define operations '())

;; interp-program :: [Instruction] -> Value
;;
;; Executes the program, which is a list of whitespace instructions. When
;; the program terminates, evaluates to the top value in the value stack.
(define (interp-program program)
  (let* ([final-state (call/cc
                       (λ (k)
                         (let ([current-state (make-state program '() '() '() 0 k)])
                           (run current-state))))]
         [value-stack (state-value-stack final-state)])
    (if (empty? value-stack)
        'finished
        (car value-stack))))

;; run :: State -> State
;;
;; Executes the next command from current-state, and returns a final state upon the
;; program's termination. The two terminating conditions are: a program counter 
;; exceeds the number of instructions, or an explicit (end) command, handled by the
;; continuation set in interp-program.
(define (run current-state)
  (let* ([pc (state-program-counter current-state)]
         [prog (state-program current-state)])
    (cond
      [(< pc (length prog))
       (let ([new-state (run-instruction current-state (list-ref prog pc))])
         (run new-state))]
      [else
       ((state-escape current-state) current-state)])))

;; print-state :: State -> ()
;;
;; Mostly for debugging: prints the components of a state.
(define (print-state a-state)
  (match a-state
    ([struct state (prog vs cs mem pc k)]
     (printf "------STATE~nprog = ~v~nvs = ~v~ncs = ~v~nmem = ~v~npc = ~v~n------~n" prog vs cs mem pc))))


;; run-instruction :: State * Instruction -> State
;;
;; Finds the appropriate operation from the macro-ed definitions, performs it.
(define (run-instruction current-state instr)
  (let ([result (ormap (λ (k) (k current-state instr)) operations)])
    (if result
        result
        (begin
          (printf "Error! No matching clause for command ~v~n" instr)
          (printf "Ending program...~n")
          ((state-escape current-state) 'failure)))))
  

;                                                                                            
;                                                                                            
;                                                                                            
;                                                                                            
;                                                            ;                               
;                                                  ;;        ;                               
;                                                  ;                                         
;     ;;;    ;  ;;      ;;;     ;  ;;     ;;;    ;;;;;;    ;;;       ;;;    ;; ;;      ;;;   
;    ;  ;;   ;;;  ;    ;   ;    ;;;  ;   ;   ;     ;         ;      ;  ;;   ;;;  ;    ;   ;  
;   ;    ;;  ;    ;;  ;    ;;   ;;           ;;    ;         ;     ;    ;;  ;;   ;    ;      
;   ;     ;  ;     ;  ;;;;;;;   ;            ;;    ;         ;     ;     ;  ;;   ;    ;;;    
;   ;     ;  ;     ;  ;         ;        ;;;;;;    ;         ;     ;     ;  ;;   ;      ;;;  
;   ;     ;  ;     ;  ;         ;       ;    ;;    ;         ;     ;     ;  ;;   ;        ;; 
;    ;   ;   ;;   ;    ;   ;    ;       ;    ;;    ;;        ;      ;   ;   ;;   ;   ;;   ;  
;     ;;;    ; ;;;      ;;;;    ;        ;;;;;;     ;;;    ;;;;;     ;;;    ;;   ;    ;;;;   
;            ;                                                                               
;            ;                                                                               
;            ;                                                                               


(define-whitespace-operations
  ;; The Whitespace VM contains
  ;;   * A program to run.
  ;;   * A value stack to operate on.
  ;;   * A call stack for subroutines.
  ;;   * A Memory Store for heap storage.
  ;;   * A Program Counter to navigate the program.
  ;;   * An escape continuation, to end at any moment.
  (prog vstack cstack mem pc k)
  
  [(push i)
   ;; Pushes the number to the top of the value stack.
   (make-state prog (cons i vstack) cstack mem pc k)]

  [(dup)
   ;; Duplicates the value on the top of the value stack.
   (make-state prog (cons (car vstack) vstack) cstack mem pc k)]

  [(ref i)
   ;; Refer to a value i entries down the value stack.
   (make-state prog (cons (list-ref vstack i) vstack) cstack mem pc k)]

  [(slide i)
   ;; "Slide" off i values from the vstack, preserving the top.
   (match vstack
     [(list fst rst ...)
      (make-state prog (cons fst (drop rst i)) cstack mem pc k)])]

  [(swap)
   ;; Swaps the first and second value in the value stack.
   (make-state prog (cons (cadr vstack) (cons (car vstack) (cddr vstack))) cstack mem pc k)]

  [(discard)
   ;; Discards the top value in the value stack.
   (make-state prog (cdr vstack) cstack mem pc k)]

  [(infix op)
   ;; Performs 'op' on the two top values of the value stack.
   (match vstack
     [(list right left rst ...)
      (let* ([val (match
                      op
                    ['plus (+ left right)]
                    ['minus (- left right)]
                    ['times (* left right)]
                    ['divide (quotient left right)]
                    ['modulo (modulo left right)])])
        (make-state prog (cons val rst) cstack mem pc k))])]
  
  [(read-char)
   ;; Reads a char from the current input port, stores it in the heap store.
   (match vstack
     [(list loc rst ...)
      (let ([datum (char->integer (car (string->list (symbol->string (read)))))])
        (make-state prog rst cstack (store-in-heap mem datum loc) pc k))])]

  [(output-char)
   ;; Outputs the top value of the value stack, as a char.
   (printf "~a" (integer->char (car vstack)))
   (make-state prog (cdr vstack) cstack mem pc k)]

  [(read-int)
   ;; Read an int, and store it in the heap store in the address at the top of the value stack.
   (let ([datum (read)])
     (make-state prog (cdr vstack) cstack (store-in-heap mem datum (car vstack)) pc k))]

  [(output-int)
   ;; Outputs the top value of the value stack, as an int.
   (printf "~a" (car vstack))
   (make-state prog (cdr vstack) cstack mem pc k)]

  [(label l)
   ;; Create a label in the program, for reference by 'call' or 'if'
   (make-state prog vstack cstack mem pc k)]

  [(call l)
   ;; Execute code located after label 'l', then return to current position.
   (let ([index (find-label prog l)])
     (make-state prog vstack (cons pc cstack) mem index k))]

  [(jump l)
   ;; Jump to the code located after label 'l'
   (let ([index (find-label prog l)])
     (make-state prog vstack cstack mem index k))]

  [(if t l)
   ;; Checks whether the top value in the stack matches the condition. 'if'
   ;; has two forms:
   ;;   * (if zero label) -> true if top value in value stack == 0
   ;;   * (if negative label) -> true if top value in value stack < 0
   ;;
   ;; In the case of true, go to label. false, we continue.
   (let ([fst (car vstack)]
         [rst (cdr vstack)])
     (if (or (and (eq? t 'zero) (= fst 0))
             (and (eq? t 'negative) (< fst 0)))
         (let ([index (find-label prog l)])
           (make-state prog rst cstack mem index k))
         (make-state prog rst cstack mem pc k)))]

  [(return)
   ;; Returns to the top value in the call stack, often from the 'call' instruction.
   (make-state prog vstack (cdr cstack) mem (car cstack) k)]
  
  [(end)
   ;; Ends the program being interpreted.
   (k (make-state prog vstack cstack mem cstack k))]

  [(store)
   ;; Stores the value at the top of the value stack in the heap store,
   ;; at the location specified by the second value in the value stack.
   (let ([new-heap (store-in-heap mem (car vstack) (cadr vstack))])
     (make-state prog (cddr vstack) cstack new-heap pc k))]

  [(retrieve)
   ;; Retrieves the value in the heap store, denoted at the location at
   ;; the top of the value stack.
  (let ([value (retrieve-from-heap mem (car vstack))])
    (make-state prog (cons value (cdr vstack)) cstack mem pc k))])


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

;; increment-pc :: State -> State
;;
;; Increments the program counter on the current state.
(define (increment-pc current-state)
  (match current-state
    [(state prog vs cs mem pc k) 
     (make-state prog vs cs mem (add1 pc) k)]))

;; store-in-heap :: Heap * Datum * Integer -> Heap
;;
;; Returns a new heap with the data stored in the appropriate location.
(define (store-in-heap a-heap datum loc)
  (cond
    [(empty? a-heap)
     (cond
       [(= 0 loc) (cons datum '())]
       [else (cons 0 (store-in-heap a-heap datum (sub1 loc)))])]
    [else
     (cond
       [(= 0 loc) (cons datum (cdr a-heap))]
       [else (cons (car a-heap) (store-in-heap (cdr a-heap) datum (sub1 loc)))])]))

;; retrieve-from-heap :: Heap * Integer -> Value
;;
;; Retrieves a value from the heap at a certain offset.
(define (retrieve-from-heap a-heap loc)
  (list-ref a-heap loc))

;; find-label :: Program * Label -> Integer
;;
;; Given a program and a label, will find the '(label LABEL) offset in the program.
(define (find-label a-program label)
  (letrec ([recur (λ (prog accum)
                    (cond
                      [(empty? prog) 'not-found]
                      [(and (list? (car prog))
                            (eq? 'label (caar prog))
                            (eq? label (cadar prog)))
                       accum]
                      [else (recur (cdr prog) (add1 accum))]))])
    (recur a-program 0)))



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


(define (math-check msg prog num)
  (check-equal? (interp-program prog) num msg))

;; Stack and arithmetic - push, dup, swap, discard, infix, ref, slide

(math-check
 "push, plus, discard"   ;; vstack -> '()
 '((push 14)             ;;            (14)
   (push 11)             ;;            (11 14)
   (push 45)             ;;            (45 11 14)
   (discard)             ;;            (11 14)
   (infix plus)) 25)     ;;            (25)

(math-check
 "dup, swap, minus"      ;; vstack -> '()
 '((push 11)             ;;            (11)
   (dup)                 ;;            (11 11)
   (infix times)         ;;            (121)
   (push 221)            ;;            (221 121)
   (swap)                ;;            (121 221)
   (infix minus)) 100)   ;;            (100)

(math-check
 "ref, divide"
 '((push 10)
   (ref 0)
   (infix divide)   ;; should be '(1)
   (push 7)
   (push 5)
   (infix divide)   ;; should be '(1 1)
   (infix plus)) 2)

(math-check
 "ref, modulo"
 '((push 4)        ;; (4)
   (push 3)        ;; (3 4)
   (push 9)        ;; (9 3 4)
   (push 7)        ;; (7 9 3 4)
   (push 5)        ;; (5 7 9 3 4)
   (ref 2)         ;; (9 5 7 9 3 4)
   (ref 2)         ;; should be '(7 9 5 7 9 3 4)
   (infix modulo)) 2)

(math-check
 "slide (simple)"
 '((push 14)
   (push 12)
   (push 10)
   (push 1)
   (slide 2)
   (infix plus)) 15)

(math-check
 "slide (twice)"
 '((push 5)
   (push 1)
   (push 1)
   (push 1)
   (push 15)
   (push 1)
   (push 1)
   (push 1)
   (push 15)
   (slide 3)
   (infix plus)
   (slide 3)
   (infix plus)) 35)

;; Control functions. - label, call, jump, if, return, implicit/explicit end.

(math-check
 "control functions, no if."
 '((push 15)             ;; vstack -> '(15)
   (call push-25)        ;;            (25 15)
   (call push-10)        ;;            (10 25 15)
   (infix plus)          ;;            (35 15)
   (infix plus)          ;;            (50)
   (end)                 ;;            *end program -> 50*
   (label push-25)       ;;
   (push 11)             ;;            (11 ...)
   (push 14)             ;;            (14 11 ...)
   (infix plus)          ;;            (25 ...)
   (return)              ;;
   (label sub-7)         ;;            Never called, included to ensure its presence doesn't disturb other labels.
   (push 7)              ;;            (7 ...)
   (infix minus)         ;;            Not being able to write this value is where stack languages get their POWAAAAAA
   (end)                 ;;
   (label push-10)       ;;
   (push 15)             ;;            (15 ...)
   (push 5)              ;;            (5 15 ...)
   (infix minus)         ;;            (10 ...)
   (return)) 50)

(math-check
 "control functions, with if"
 '((push 10)
   (push 10)
   (infix minus)
   (if zero output-15)
   (push 1)
   (jump end-if)
   (label output-15)
   (push 15)
   (label end-if)
   (end)) 15)

(math-check
 "control functions, with if (testing else case)"
 '((push 10)
   (push 5)
   (infix minus)
   (if zero output-15)
   (push 1)
   (jump end-if)
   (label output-15)
   (push 15)
   (label end-if)
   (end)) 1)

(math-check
 "control functions with neg rather than 0"
 '((push 10)
   (push 11)
   (infix minus)
   (if negative push-30)
   (push 1)
   (jump after-if)
   (label push-30)
   (push 30)
   (label after-if)
   (push 20)
   (push 19)
   (infix minus)
   (if negative push-1)
   (push 30)
   (jump sum-it)
   (label push-1)
   (push 1)
   (label sum-it)
   (infix plus)) 60)

;; Heap - store and retrieve
(math-check
 "storage and retrieval from the heap, all in-bounds"
 '((push 0)
   (push 200)
   (store)
   (push 11)
   (dup)
   (infix plus)
   (push 0)
   (retrieve)
   (infix plus)
   (push 1)
   (swap)
   (store)
   (push 0)
   (retrieve)
   (push 1)
   (retrieve)
   (infix plus)) 422)

(math-check
 "storage and retrieval, with some out-of-bounds storage values"
 '((push 10)
   (push 200)
   (store)
   (push 14)
   (push 8)
   (retrieve)
   (infix plus) ;; should be 14 + 0
   (push 10)
   (retrieve)
   (infix plus)) 214)

;; output-char
(define (check-output prog expected msg)
  (let ([output (with-output-to-string
               (λ ()
                 (interp-program prog)))])
    (check-equal? output expected msg)))

(check-output
 `((push ,(char->integer #\d))
   (push ,(char->integer #\l))
   (push ,(char->integer #\r))
   (push ,(char->integer #\o))
   (push ,(char->integer #\w))
   (push ,(char->integer #\space))
   (push ,(char->integer #\o))
   (push ,(char->integer #\l))
   (push ,(char->integer #\l))
   (push ,(char->integer #\e))
   (push ,(char->integer #\h))
   (output-char)
   (output-char)
   (output-char)
   (output-char)
   (output-char)
   (output-char)
   (output-char)
   (output-char)
   (output-char)
   (output-char)
   (output-char))
 "hello world"
 "Print char")

(check-output
 '((push 11)
   (push 45)
   (push 4)
   (output-int)
   (output-int)
   (output-int))
 "44511"
 "Print int")

(define (check-input prog str expected msg)
  (let ([result (with-input-from-string
                 str
                 (λ ()
                   (interp-program prog)))])
    (check-equal? result expected msg)))

(check-input
 '((push 11)
   (push 0)
   (read-int)
   (push 0)
   (retrieve)
   (infix plus))
 "14"
 25
 "Read int")

(check-input
 `((push ,(char->integer #\k)) ;; 'k' is 107 in ASCII
   (push 0)
   (read-char)
   (push 0)
   (retrieve)
   (infix plus))
 "o"                           ;; 'o' is 111
 218
 "Read char")