#lang racket
(require nanopass/base)

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
                    `(if ,e0 ,e1 ,e2))))


(define-parser parse-L0 L0)

(define +word-size+ 4)

(define (L1->x86 expr env)
  (define (lookup e)
    (index-of env e))
  (match expr
    ((? symbol?)
     (printf "movl %eax, ~a(%ebp)\n" (- (* +word-size+ (add1 (lookup expr))))))
    ((? number?)
     (printf "movl $~a, %eax\n" expr))
    ((? boolean?)
     (printf "movl $~a, %eax\n" (if expr 1 0)))
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
      (let ((slot (length env)))
        (printf "movl %eax, ~a(%ebp)\n" (- (* +word-size+ (add1 slot))))
        (L1->x86 body (cons n env)))
      )))

(printf ".global fir_entry\n")
(printf ".type fir_entry @function\n")
(printf ".align 8\n")
(printf "fir_entry:\n")
(printf "pushl %ebp\n")
(printf "movl %esp, %ebp\n")
(L1->x86 (unparse-L1 (type-check-and-discard-type-info (parse-L0 '(let f bool #f (if f 2 3))))) '())
(printf "movl %ebp, %esp\n")
(printf "popl %ebp\n")
(printf "ret\n")
