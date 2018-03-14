#lang racket
(require racket/cmdline)

(require "fir.rkt")

(define f
  (command-line
    #:program "compiler"
    #:args (filename)
    filename))

(fir-compile (with-input-from-file f (thunk (read))))

