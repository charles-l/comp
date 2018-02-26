#lang racket

; LINKS
; * http://web.cs.ucla.edu/~palsberg/tba/
; * http://www.ccs.neu.edu/home/shivers/papers/diss.pdf
; * http://www.ccs.neu.edu/home/shivers/papers/trec.pdf
; * http://www.ccs.neu.edu/home/shivers/citations.html
; * https://www.cs.purdue.edu/homes/suresh/590s-Fall2002/papers/Orbit.pdf
; * http://web.cs.ucla.edu/~palsberg/tba/papers/dimock-muller-turbak-wells-icfp97.pdf
;
; TODO
; * deforestation
; * inlining
; * partial evaluation
; * common subexpression
; * loop invarient detection
; * range analysis
; * redundent assignment
; * dead code elimination
; * constant propagation
; * code hoisting
; * loop unrolling

(require sugar)

(define +word-size+ 4)
(define *regs* '(eax esp ebx edx ebp))
(define *binops* #hash((+ . addl)
                       (- . subl)
                       (* . imul)))

(define *tags* #hash((int . 0)
                     (bool . 1)))

(struct binding (name type))

(define *functions* '())

(define (reg? r) (memq r *regs*))
(define env? (listof (hash/c symbol? (cons/c binding? integer?))))
(define (literal? l) (integer? l))
(define (offset? p)
  (and (pair? p) (integer? (car p)) (integer? (cdr p))))

(define (immediate? x) (or (literal? x) (symbol? x) (boolean? x)))

(provide
  (contract-out
    (compile (-> (or/c list? number?) env?
                 (listof (listof string?))))
    (reg->string (-> reg? string?))
    (literal->string (-> literal? string?))
    (offset->string (-> offset? string?))
    (env-frame-size (-> env? integer?))
    (env-bind-local (-> env? symbol? (values env? integer?)))
    (env-lookup (-> env? symbol? (cons/c integer? (cons/c binding? integer?))))
    (compile-imm (-> immediate? env? list?))))

(define (label s)
  (list (string-append (->string s) ":")))

(define (env-frame-size h)
  (count (compose negative? cdr cdr) (hash->list h)))

; TODO rename these to something saner
(define (name:type-type n)
  (let ((l (string-split (symbol->string n) ":")))
   (if (= 1 (length l))
       'any
       (string->symbol (cadr l)))))
(define (name:type-name n)
  (string->symbol (car (string-split (symbol->string n) ":"))))

(define (env-bind-local env name:type)
  (let ((slot (add1 (env-frame-size (car env))))
        (name (name:type-name name:type))
        )
   (values
     (cons (hash-set (car env) name
                     (cons (binding name
                                    (name:type-type name:type)) slot))
           (cdr env))
     (cons 0 slot))))

(define (env-lookup env name)
  (let l ((e env) (i 0))
   (if (null? e)
       (error "no such binding:" name)
       (let ((v (hash-ref (car e) name #f)))
        (if v
            (cons i v)
            (l (cdr e) (add1 i)))))))

(define (env-lookup-slot env name)
  (let ((b (env-lookup env name)))
   (cons (car b) (cdr (cdr b)))))

(define (env-lookup-type env name)
  (binding-type (car (cdr (env-lookup env name)))))

(define (env-bind-args env args)
  (cons
    #hash()
    (cons (for/hash ((a args) (i (in-naturals 2)))
            (values (name:type-name a) (cons (binding (name:type-name a) (name:type-type a)) i)))
          env)))

(define (reg->string r)
  (string-append "%" (symbol->string r)))

(define (literal->string l)
  (string-append "$" (number->string l)))

(define (offset->string o)
  (cond ((zero? (car o)) ; in current frame
         (string-append (number->string (- (* +word-size+ (cdr o)))) "(%ebp)"))
        ((eq? (car o) 1) ; argument
         (string-append (number->string (* +word-size+ (cdr o))) "(%ebp)"))
        (else (error "unhandled offset" o))))

; TODO: write macro to generate this
(define *instructions* (hash
                         'movl (match-lambda*
                                 (`(,(or (? literal? a) (? reg? a) (? offset? a) (? string? a)) ,(or (? reg? b) (? offset? b)))
                                   (list "movl" a b)))
                         'addl (match-lambda*
                                 (`(,(or (? literal? a) (? reg? a) (? offset? a)) ,(or (? reg? b)))
                                   (list "addl" a b)))
                         'subl (match-lambda*
                                 (`(,(or (? literal? a) (? reg? a) (? offset? a)) ,(or (? reg? b)))
                                   (list "subl" a b)))
                         'imul (match-lambda*
                                 (`(,(or (? literal? a) (? reg? a) (? offset? a)) ,(or (? reg? b)))
                                   (list "imul" a b)))
                         'idiv (match-lambda* ; needs divisor in %edx:%eax
                                 (`(,(or (? literal? a) (? reg? a) (? offset? a)))
                                   (list "idiv" a)))
                         'call (match-lambda*
                                 (`(,s)
                                   (list "call" s)))
                         'pushl (match-lambda*
                                  (`(,s)
                                    (list "pushl" s)))
                         'popl (match-lambda*
                                 (`(,s)
                                   (list "popl" s)))))

(define (inst v . args)
  (apply (hash-ref *instructions* v) args))

(define (with-tag t val)
  (bitwise-ior (arithmetic-shift val 1)
               (hash-ref *tags* t)))

(define (compile-imm i env #:boxed (do-box? #f))
  (define (maybe-tag t v)
    (if do-box?
        (with-tag t v)
        v))
  (match i
    ((? symbol? s) (env-lookup-slot env s))
    ((? literal? d) (maybe-tag 'int d))
    ((? boolean? b) (maybe-tag 'bool (if b (arithmetic-shift 1 31) 0)))))

(define (compile expr env)
  (define (expect-type! expected val)
    (match val
      ((? symbol?)
       (define valt (env-lookup-type env val))
       (unless (eq? valt expected)
         (error "expected variable" val "to be type" expected "but was" valt)))
      ((? literal?)
       (unless (eq? expected 'int) (error "unexpected literal" val)))
      ((? boolean?)
       (unless (eq? expected 'bool) (error "unexpected boolean" val)))))

  (match expr
    ((? immediate? i)
     (list (inst 'movl (compile-imm i env) 'eax)))
    (`(,(? (curry hash-has-key? *binops*) o) ,(? immediate? a) ,(? immediate? b))
      (expect-type! 'int a)
      (expect-type! 'int b)
      (list (inst 'movl (compile-imm a env) 'eax)
            (inst (hash-ref *binops* o) (compile-imm b env) 'eax)))
    (`(/ ,(? immediate? a) ,(? immediate? b))
      (expect-type! 'int a)
      (expect-type! 'int b)
      (list (inst 'movl (compile-imm a env) 'eax)
            (inst 'movl b 'ebx)
            (inst 'movl 0 'edx)
            (inst 'idiv 'ebx)))
    (`(if ,(? immediate? cond) ,then-expr ,else-expr)
      (expect-type! 'bool cond)
      (let ((else-label (gensym 'else))
            (end-label (gensym 'end)))
        (append
          (list (inst 'movl (compile-imm cond env) 'eax))
          '(("cmp" "$0" "%eax"))
          `(("jz" ,else-label))
          (compile then-expr env)
          `(("jmp" ,end-label))
          `(,(label else-label))
          (compile else-expr env)
          `(,(label end-label)))))
    (`(let ,name:type ,val ,body)
      (let-values (((new-env slot) (env-bind-local env name:type)))
        (append
          (compile val env)
          (list (inst 'movl 'eax slot))
          (compile body new-env))))
    (`(λ ,args ,body)
      ; TODO check types of function
      (let ((l-name (gensym 'lambda)))
       (compile-function! l-name body (env-bind-args env args))
       (list (inst 'movl (~a "$" l-name) 'eax))))
    (`(,f ,args ...)
      ; TODO check argument types
      (append (compile f env)
              (append
                (map (curry inst 'pushl)
                     (map (curryr compile-imm env) (reverse args)))
                (list (inst 'call "*%eax")))))))

(define (asmthing->string x)
  (cond ((reg? x) (reg->string x))
        ((literal? x) (literal->string x))
        ((offset? x) (offset->string x))
        ((string? x) x)
        ((symbol? x) (symbol->string x))
        (else
          (error "can't convert to string:" x))))


(define (print-asm instruction)
  (let ((instruction* (map asmthing->string instruction)))
   (displayln
     (cond ((= 3 (length instruction*))
            (string-append (car instruction*)
                           " "
                           (string-join (cdr instruction*) ",")))
           ((= 2 (length instruction*)) (string-join instruction* " "))
           (else
             (car instruction*))))))

(define (compile-function! name code env)
  (set! *functions* (cons (cons name (compile code env)) *functions*)))

(define (emit-functions)
  (for ((f *functions*))
    (match-let (((cons name code) f))
      (displayln (~a ".global " name))
      (displayln (~a ".type " name " @function"))
      (displayln ".align 8")
      (displayln (~a name ":"))
      (displayln "pushl %ebp")
      (displayln "movl %esp, %ebp")
      (map print-asm code)
      (displayln "movl %ebp, %esp")
      (displayln "popl %ebp")
      (displayln "ret"))))

(compile-function! "fir_entry" '(let f
                                 (λ (a:int) (+ a 2))
                                 (let g:int (f 1)
                                  (if #t
                                      (+ g 3)
                                      0))) (list #hash()))
(emit-functions)
