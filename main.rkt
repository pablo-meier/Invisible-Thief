#lang racket

(require "semantics.rkt"
         "parser.rkt")

(define (main)
  (let* ([args (current-command-line-arguments)]
         [filename (vector-ref args 0)])
    (call-with-input-file filename
      (λ (file-input)
        (interp-program (parse #f file-input)))
      #:mode 'binary)))

(main)
