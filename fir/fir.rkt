#lang racket

(require sugar)

(define +word-size+ 4)
(define *regs* '(eax esp ebx edx))

(define (reg? r) (memq r *regs*))
(define env? (hash/c symbol? integer?))
(define (literal? l) (integer? l))
(define (offset? p)
  (and (pair? p) (reg? (car p)) (integer? (cdr p))))

(define (immediate? x) (or (literal? x) (symbol? x)))

(provide
  (contract-out
    (compile (-> (or/c list? number?) env?
                 (listof (listof string?))))
    (reg->string (-> reg? string?))
    (literal->string (-> literal? string?))
    (offset->string (-> offset? string?))
    (env-size (-> env? integer?))
    (env-add (-> env? symbol? (values env? integer?)))
    (env-lookup (-> env? symbol? integer?))
    (mov-imm-to-eax (-> immediate? env? list?))))

(define (label s)
  (list (string-append (->string s) ":")))

(define (env-size h)
  (length (hash->list h)))

(define (env-add env name)
  (let ((l (add1 (env-size env))))
   (values (hash-set env name l) l)))

(define (env-lookup env name)
  (hash-ref env name))

(define (reg->string r)
  (string-append "%" (symbol->string r)))

(define (literal->string l)
  (string-append "$" (number->string l)))

(define (offset->string o)
  ; TODO: move this implicit word-size stuff up one level
  (string-append (number->string (- (* +word-size+ (cdr o)))) "(" (reg->string (car o)) ")"))

; TODO: write macro to generate this
(define *instructions* (hash
                         'movl (match-lambda*
                                 (`(,(or (? literal? a) (? reg? a) (? offset? a)) ,(or (? reg? b) (? offset? b)))
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
                         ))

(define *binops* #hash((+ . addl)
                       (- . subl)
                       (* . imul)))

(define (inst v . args)
  (apply (hash-ref *instructions* v) args))

; move a variable or literal into %eax
(define (mov-imm-to-eax i env)
  (match i
    ((? symbol? s) (inst 'movl (cons 'esp (env-lookup env s)) 'eax))
    ((? literal? d) (inst 'movl d 'eax))))

(define (compile expr env)
  (match expr
    ((? immediate? i) (list (mov-imm-to-eax i env)))
    (`(,(? (curry hash-has-key? *binops*) o) ,(? immediate? a) ,(? immediate? b))
      (list (mov-imm-to-eax a env)
            (inst (hash-ref *binops* o) b 'eax)))
    (`(/ ,(? immediate? a) ,(? immediate? b))
      (list (mov-imm-to-eax a env)
            (inst 'movl b 'ebx)
            (inst 'movl 0 'edx)
            (inst 'idiv 'ebx)))
    (`(if ,(? immediate? cond) ,then-expr ,else-expr)
      (let ((else-label (gensym 'else))
            (end-label (gensym 'end)))
        (append
          (mov-imm-to-eax cond env)
          '(("cmp" "$0" "%eax"))
          `(("jz" ,else-label))
          (compile then-expr env)
          `(("jmp" ,end-label))
          `(,(label else-label))
          (compile else-expr env)
          `(,(label end-label)))))
    (`(let ,name ,val ,body)
      (let-values (((new-env slot) (env-add env name)))
        (append
          (compile val env)
          (list (inst 'movl 'eax `(esp . ,slot)))
          (compile body new-env))))))


(define (asmthing->string x)
  (cond ((reg? x) (reg->string x))
        ((literal? x) (literal->string x))
        ((offset? x) (offset->string x))
        ((string? x) x)
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

(define (wrap code)
  (displayln ".text")
  (displayln ".global fir_entry")
  (displayln ".type fir_entry @function")
  (displayln ".align 8")
  (displayln "fir_entry:")
  (map print-asm code)
  (displayln "ret"))

(wrap (compile '(let f 1 (let g (+ f 2) (/ g 3))) #hash()))
