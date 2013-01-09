#lang racket

(require rackunit)

(provide interp-program)

;; TODO
;;
;; Remove ugly continuation hack for 'end'
;; clean up interp-program to use eval.
;; Profile?

(define namespace (current-namespace))

(define-struct state (program value-stack call-stack memory program-counter))

(define (new-state)
  (make-state '() '() '() '() 0))

(define (new-state-with-program prog)
  (make-state prog '() '() '() 0))

(define (inc-instruction a-state)
  (match a-state
    [(state p v c m pc)
     (state p v c m (add1 pc))]))

(define-syntax (define-whitespace-operation stx)
  (syntax-case stx ()
    [(_ (name-of-op params ...) (id ...) body ...)
     #'(define (name-of-op params ... a-state)
         (let ([rslt (match a-state
                       [(struct state (id ...))
                        body ...])])
           (inc-instruction rslt)))]))


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


(define-whitespace-operation (push i)
  (prog vstack cstack mem pc)
  (state prog (cons i vstack) cstack mem pc))

(define-whitespace-operation (dup)
  (prog vstack cstack mem pc)
  (state prog (cons (car vstack) vstack) cstack mem pc))

(define-whitespace-operation (ref i)
  (prog vstack cstack mem pc)
  (state prog (cons (list-ref vstack i) vstack) cstack mem pc))

(define-whitespace-operation (slide i)
  (prog (list fst rst ...) cstack mem pc)
  (state prog (cons fst (drop rst i)) cstack mem pc))

(define-whitespace-operation (swap)
  (prog (list fst snd rst ...) cstack mem pc)
  (state prog `(,snd ,fst . ,rst) cstack mem pc))

(define-whitespace-operation (discard)
  (prog vstack cstack mem pc)
  (state prog (cdr vstack) cstack mem pc))

(define-whitespace-operation (infix op)
  (prog (list right left rst ...) cstack mem pc)
  (let* ([val (match
                  op
                ['plus (+ left right)]
                ['minus (- left right)]
                ['times (* left right)]
                ['divide (quotient left right)]
                ['modulo (modulo left right)])])
    (state prog (cons val rst) cstack mem pc)))

(define-whitespace-operation (read-char)
  (prog (list loc rst ...) cstack mem pc)
  (let ([datum (char->integer (car (string->list (symbol->string (read)))))])
    (state prog rst cstack (store-in-heap mem datum loc) pc)))

(define-whitespace-operation (output-char)
  (prog (list fst rst ...) cstack mem pc)
  (printf "~a" (integer->char fst))
  (state prog rst cstack mem pc))

(define-whitespace-operation (read-int)
  (prog (list loc rst ...) cstack mem pc)
  (let ([datum (read)])
    (state prog rst cstack (store-in-heap mem datum loc) pc)))

(define-whitespace-operation (output-int)
  (prog (list fst rst ...) cstack mem pc)
  (printf "~a" fst)
  (state prog rst cstack mem pc))

(define-whitespace-operation (label l)
  (prog vstack cstack mem pc)
  (state prog vstack cstack mem pc)) ;; don't manipulate the stack, just increment pc

(define-whitespace-operation (call l)
  (prog vstack cstack mem pc)
  (let ([index (find-label prog l)])
    (state prog vstack (cons pc cstack) mem index)))

(define-whitespace-operation (jump l)
  (prog vstack cstack mem pc)
  (let ([index (find-label prog l)])
    (state prog vstack cstack mem index)))

(define-whitespace-operation (ws-if t l)
  (prog (list fst rst ...) cstack mem pc)
  (if (or (and (eq? t 'zero) (= fst 0))
          (and (eq? t 'negative) (< fst 0)))
      (let ([index (find-label prog l)])
        (state prog rst cstack mem index))
      (state prog rst cstack mem pc)))

(define-whitespace-operation (return)
  (prog vstack (list fst rst ...) mem pc)
  (state prog vstack rst mem fst))

;; using an escape continuation, but one we globally set before
;; running the interpreter. ewwwww.....
(define global-escape! #f)
(define (end a-state)
  (global-escape! a-state))

(define-whitespace-operation (store)
  (prog (list value loc rst ...) cstack heap pc)
  (let ([new-heap (store-in-heap heap value loc)])
    (state prog rst cstack new-heap pc)))

(define-whitespace-operation (retrieve)
  (prog (list value rst ...) cstack heap pc)
  (let ([value (retrieve-from-heap heap value)])
    (state prog (cons value rst) cstack heap pc)))

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

(define (find-label a-program label)
  (letrec ([recur (λ (prog lbl accum)
                    (cond
                      [(empty? prog) 'not-found]
                      [(and (list? (car prog))
                            (eq? 'label (caar prog))
                            (eq? lbl (cadar prog)))
                       accum]
                      [else (recur (cdr prog) lbl (add1 accum))]))])
    (recur a-program label 0)))


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

(define (retrieve-from-heap a-heap loc)
  (list-ref a-heap loc))

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

;; Takes a list of commands, and executes them
;; one by one, using the input of the last as the
;; input to the next. When the program counter > length of
;; the program, the program evaluates to the top of the value stack.
;;
;; Sample input would be '((push 14) (push 11) (push 45) (discard) (infix plus)) and would
;; evaluate to 25.
(define (interp-program list-of-commands)
  (let ([get-fun-from-instr (λ (name) (match name          ;; This function is stupid as hell, but
                                        ['push push]       ;; I'd rather just run this shit than spend
                                        ['dup dup]         ;; more time reading why I can't just eval this
                                        ['ref ref]         ;; without fungling around with syntax objects
                                        ['slide slide]     ;; and the like. Vim shows that power != intuitive,
                                        ['swap swap]       ;; but Christ, Racket's facilities have a deep learning curve.
                                        ['discard discard]
                                        ['infix infix]
                                        ['read-char read-char]
                                        ['output-char output-char]
                                        ['read-int read-int]
                                        ['output-int output-int]
                                        ['label label]
                                        ['call call]
                                        ['jump jump]
                                        ['return return]
                                        ['store store]
                                        ['retrieve retrieve]
                                        ['ws-if ws-if]
                                        ['end end]))])
    (letrec ([do-instr (λ (a-state)
                         (let* ([instr (match a-state
                                         [(state prog vstack _ _ pc)
                                          (list-ref prog pc)])]
                                [fun (get-fun-from-instr (car instr))]
                                [args (append (cdr instr) (list a-state))])
                           (apply fun args)))])
      (let ([final-state (call/cc (λ (k)
                                    (set! global-escape! k)
                                    (let ([starting (new-state-with-program list-of-commands)])
                                      (letrec ([recur (λ (new-state)
                                                        (if (>= (state-program-counter new-state)
                                                               (length (state-program new-state)))
                                                            new-state
                                                            (recur (do-instr new-state))))])
                                        (recur starting)))))])
        (let ([vstate (state-value-stack final-state)])
          (if (empty? vstate)
              '()
              (car vstate)))))))

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
 '((push 15)            ;; vstack -> '(15)
   (call push-25)       ;;            (25 15)
   (call push-10)       ;;            (10 25 15)
   (infix plus)         ;;            (35 15)
   (infix plus)         ;;            (50)
   (end)                ;;            *end program -> 50*
   (label push-25)      ;;
   (push 11)            ;;            (11 ...)
   (push 14)            ;;            (14 11 ...)
   (infix plus)         ;;            (25 ...)
   (return)             ;;
   (label sub-7)        ;;            Never called, included to ensure its presence doesn't disturb other labels.
   (push 7)             ;;            (7 ...)
   (infix minus)        ;;            Not being able to write this value is where stack languages get their POWAAAAAA
   (end)                ;;
   (label push-10)      ;;
   (push 15)            ;;            (15 ...)
   (push 5)             ;;            (5 15 ...)
   (infix minus)        ;;            (10 ...)
   (return)) 50)

(math-check
 "control functions, with if"
 '((push 10)
   (push 10)
   (infix minus)
   (ws-if zero output-15)
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
   (ws-if zero output-15)
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
   (ws-if negative push-30)
   (push 1)
   (jump after-if)
   (label push-30)
   (push 30)
   (label after-if)
   (push 20)
   (push 19)
   (infix minus)
   (ws-if negative push-1)
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