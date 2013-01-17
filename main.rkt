#lang racket

(require "semantics.rkt"
         "parser.rkt")

(define (main)
  (let* ([args (current-command-line-arguments)]
         [filename (vector-ref args 0)])
    (call-with-input-file filename
      (λ (file-input)
        (let ([prog (parse #f file-input)])
          (if prog
              (interp-program prog)
              #f)))
      #:mode 'binary)
    (void)))

(main)
