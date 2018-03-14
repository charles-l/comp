#lang racket
(require nanopass/base)
(require (only-in srfi/1 iota))

(provide compile)

(struct binding (name type))

(define variable? symbol?)
(define (type? x)
  (match x
    ((? symbol?) #t)
    (`(-> ,_ ...) #t)
    (else #f)))
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
                       (let x t e
                        body)
                       (e0 e1 ...)))

(define-language L1
                 (extends L0)
                 (terminals
                   (- (type (t))))
                 (Expr (e body)
                       (- (λ t (x* ...) body)
                          (let x t e body))
                       (+ (λ (x* ...) body)
                          (let x e body))))

(define-pass type-check-and-discard-type-info : L0 (ir) -> L1 ()
             (definitions
               (define (check env x t)
                 (let ((xt (infer x env)))
                   (unless (equal? xt t)
                     (error "expected" x "to be of type" t "but was" xt))
                   xt)))
             (infer : Expr (ir env) -> * (type)
                    (,x (env-lookup-type env x))
                    (,c (match c
                          ((? number?) 'int)
                          ((? boolean?) 'bool)
                          ((? char?) 'char)))
                    ((begin ,e* ... ,e) (infer e env))
                    ; FIXME (return union type)
                    ((if ,e0 ,e1 ,e2)
                     (check env e0 'bool)
                     (infer e1 env))
                    ((λ ,t (,x* ...) ,body)
                     (let ((env* (foldl
                                   (lambda (c e) (env-add e (car c) (cdr c)))
                                   env
                                   (map (lambda (n t) (cons n (binding n t)))
                                        x*
                                        (drop-right (cdr t) 1)))))
                       (check env* body (last t))
                       t))
                    ((let ,x ,t ,e ,body)
                     (let ((env* (env-add env x (binding x t))))
                       (check env* e t)
                       (infer body env*)))
                    ((,e0 ,e1 ...)
                     (let ((fty (infer e0 env)))
                       (unless (equal? (drop-right (cdr fty) 1) (map (curryr infer env) e1))
                         (error "function has incorrect type - expecting" fty))
                       (last fty))))
             (Expr : Expr (ir env) -> Expr ()
                   ((let ,x ,t ,[e] ,body)
                    `(let ,x ,e
                       ,(Expr body (env-add env x (binding x t)))))
                   ((λ ,t (,x* ...) ,[body])
                    `(λ (,x* ...) ,body)))
             (infer ir #hash())
             (Expr ir #hash()))

(define +word-size+ 4)

;; TODO FIXME make this pass work
(define-pass emit-asm : L1 (ir) -> * ()
             (definitions
               (define (frame-locals frame)
                 (filter (compose positive? cdr) frame))

               (define (slot-index frame e)
                 (cdr (findf (lambda (x) (eq? (car x) e)) frame)))

               (define (emit-immediate frame e)
                 (match e
                   ((? symbol?) (format "~a(%ebp)" (- (* +word-size+ (slot-index frame e)))))
                   ((? number?) (format "$~a" e))
                   ((? boolean?) (format "$~a" (if e 1 0))))))

             (Expr : Expr (ir (frame '())) -> * ()
                   (,x (printf "movl ~a, %eax\n" (emit-immediate frame x)))
                   (,c (printf "movl ~a, %eax\n" (emit-immediate frame c)))
                   ((begin ,e* ... ,e) (begin
                                         (map (curryr Expr frame) e*)
                                         (Expr e frame)))
                   ((if ,e0 ,e1 ,e2)
                    (let ((else-label (gensym 'else)) (end-label 'end))
                      (Expr e0 frame)
                      (printf "cmp $0, %eax\n")
                      (printf "jz ~a\n" else-label)
                      (Expr e1 frame)
                      (printf "jmp ~a\n" end-label)
                      (printf "~a:\n" else-label)
                      (Expr e2 frame)
                      (printf "~a:\n" end-label)))
                   ((let ,x ,e ,body)
                    (printf "subl $4, %esp\n")
                    (Expr e frame)
                    (let ((slot (add1 (length (frame-locals frame)))))
                      (printf "movl %eax, ~a(%ebp)\n" (- (* +word-size+ slot)))
                      (Expr body (cons (cons x slot) frame))))
                   ; TODO convert lambdas to labels
                   ; TODO/FIXME raise labels to top level so we can generate all funcs
                   ((λ (,x* ...) ,body)
                    (let ((n (gensym 'lambda)))
                      ; err - do something here...
                      (printf "movl $~a, %eax\n" n)))
                   ((,e0 ,e1 ...)
                    (Expr e0 frame)
                    (for-each (lambda (v)
                                (printf "pushl ~a\n" (emit-immediate frame v)))
                              e1)
                    (printf "call *%eax\n"))))

(define-parser parse-L0 L0)

;;; BEGIN CRAPPY x86 EMIT CODE TO BE DELETED ONCE EMIT-ASM WORKS { ;;;
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

;;; } END CRAPPY x86 EMIT CODE ;;;

(define (compile l)
  (emit-function "fir_entry"
                 ((compose
                    unparse-L1
                    type-check-and-discard-type-info
                    parse-L0)
                  l) '())
  (for-each (lambda (x) (emit-function (first x) (second x) (third x))) *function-queue*))

(compile '(let f bool #t (if f 2 1)))
