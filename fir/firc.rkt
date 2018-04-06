#lang racket
(require racket/cmdline)

(require "fir.rkt")

(define dump-asm (make-parameter #f))

(define f
  (command-line
    #:program "compiler"
    #:once-each (("-d") "dump each pass and assembly to stdout rather than compiling with gcc" (dump-asm #t))
    #:args (filename)
    filename))

(define (do-compile)
  (fir-compile (with-input-from-file f (thunk (read)))))

(cond
  ((dump-asm)
   (parameterize ((dump? #t))
     (do-compile)))
  (else
    (with-output-to-file "out.s" #:exists 'replace
                         (thunk (do-compile)))
    (system "gcc -m32 boot.c out.s -o p")))

