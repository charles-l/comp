#lang racket
(require nanopass/base)
(require (only-in srfi/1 iota))

(struct binding (name type))

(define variable? symbol?)
(define type? symbol?)
(define (constant? x) (or (number? x) (char? x) (boolean? x)))

(define (env-lookup env v)
  (hash-ref env v (lambda () (error "not bound" v))))

(define (env-lookup-type env v)
  (binding-type (env-lookup env v)))

(define env-add hash-set)

(define-language L0
                 (terminals
                   (variable (x))
                   (constant (c))
                   (type (t)))
                 (Expr (e body)
                       x
                       c
                       (begin e* ... e)
                       (if e0 e1 e2)
                       (λ t (x* ...) body)
                       (let x t c
                        body)
                       (e0 e1 ...)))

(define-language L1
                 (extends L0)
                 (terminals
                   (- (type (t))))
                 (Expr (e body)
                       (- (λ t (x* ...) body)
                          (let x t c body))
                       (+ (λ (x* ...) body)
                          (let x c body))))

(define-pass type-check-and-discard-type-info : L0 (e) -> L1 ()
             (definitions)
             (Expr : Expr (e (env #hash())) -> Expr ()
                   ((let ,x ,t ,c ,body)
                    `(let ,x ,c
                      ,(Expr body (env-add env x (binding x t)))))
                   ((if ,e0 ,e1 ,e2)
                    (unless (eq? (env-lookup-type env e0) 'bool)
                      (error "if condition must be of type bool"))
                    `(if ,(Expr e0 env) ,(Expr e1 env) ,(Expr e2 env)))))


(define-parser parse-L0 L0)

(define +word-size+ 4)

(define *function-queue* '())

(define (emit-function name body args)
  (printf ".global ~a\n" name)
  (printf ".type ~a @function\n" name)
  (printf ".align 8\n")
  (printf "~a:\n" name)
  (printf "pushl %ebp\n")
  (printf "movl %esp, %ebp\n")
  (L1->x86 body (map
                  (lambda (a i)
                    (cons a (- i)))
                  args
                  (iota (length args) 2)))
  (printf "movl %ebp, %esp\n")
  (printf "popl %ebp\n")
  (printf "ret\n"))

(define (L1->x86 expr env)
  (define (frame-locals)
    (filter (compose positive? cdr) env))

  (define (slot-index e)
    (cdr (findf (lambda (x) (eq? (car x) e)) env)))

  (define imm? (disjoin symbol? number? boolean?))

  (define (imm->x86 e)
    (match e
      ((? symbol?) (format "~a(%ebp)" (- (* +word-size+ (slot-index e)))))
      ((? number?) (format "$~a" e))
      ((? boolean?) (format "$~a" (if e 1 0)))))

  (match expr
    ((? imm?) (printf "movl ~a, %eax\n" (imm->x86 expr)))
    (`(if ,imm ,then-expr ,else-expr)
      (let ((else-label (gensym 'else)) (end-label 'end))
        (L1->x86 imm env)
        (printf "cmp $0, %eax\n")
        (printf "jz ~a\n" else-label)
        (L1->x86 then-expr env)
        (printf "jmp ~a\n" end-label)
        (printf "~a:\n" else-label)
        (L1->x86 else-expr env)
        (printf "~a:\n" end-label)))
    (`(let ,n ,v ,body)
      (printf "subl $4, %esp\n") ; allocate space for new value
      (L1->x86 v env)
      (let ((slot (add1 (length (frame-locals)))))
        (printf "movl %eax, ~a(%ebp)\n" (- (* +word-size+ slot)))
        (L1->x86 body (cons (cons n slot) env)))
      )
    (`(λ ,args ,body)
      (let ((n (gensym 'lambda)))
        (set! *function-queue* (append *function-queue* (list (list n body args))))
        (printf "movl $~a, %eax\n" n)))
    (`(,f ,args ...)
      (L1->x86 f env)
      (for-each (lambda (v)
                  (printf "pushl ~a\n" (imm->x86 v)))
                args)
      (printf "call *%eax\n"))))


(emit-function
  "fir_entry"
  (unparse-L1
    (type-check-and-discard-type-info
      (parse-L0
        '(let f (λ (x) x)
           (let g bool #t
             (if g
               (f 2)
               (f 3)))))))
  '())

(for-each (lambda (x) (emit-function (first x) (second x) (third x))) *function-queue*)
