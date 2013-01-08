#lang racket

(require "semantics.rkt"
         "parser.rkt")

(define (main)
  (let* ([args (current-command-line-arguments)]
         [filename (vector-ref args 0)])
    (printf "filename is ~a and string? is ~a~n" filename (string? filename))
    (call-with-input-file filename
      (λ (file-input)
        (interp-program (parse #f file-input)))
      #:mode 'binary)))

(main)
