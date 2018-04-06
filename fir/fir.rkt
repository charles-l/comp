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
; * register allocation
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

(require nanopass/base)
(require (only-in srfi/1 iota))
(require (only-in srfi/13 string-drop))
(require sugar)
(provide fir-compile dump?)

(define +word-size+ 4)
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

(define (%name? s)
  (and (variable? s) (eq? #\% (string-ref (symbol->string s) 0))))

(define (clean-%name s)
  (string-drop (symbol->string s) 1))

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

(define-language L1.1 (extends L1))

(define-language L2
                 (extends L1.1)
                 (terminals)
                 (Expr (e body)
                       (- (λ (x* ...) body))
                       (+ (lref x)) ; reference to label
                       (+ (label x (x* ...) body))
                       (+ (program e* ...))))

; read input sexp, rename variables to ensure names are unique (α-conversion)
; also desugar
(define-pass parse-and-desugar : * (e) -> L0 ()
             (definitions
               (define (in-env? env e)
                 (hash-has-key? env e))
               (define (extend-env env e)
                 (if (in-env? env e)
                   (hash-set env e (add1 (hash-ref env e)))
                   (hash-set env e 0)))
               (define (var-name env e)
                 (if (%name? e)
                   e
                   (string->symbol
                     (string-append
                       (symbol->string e)
                       "."
                       (number->string (hash-ref env e))))))
               (define (maybe-beginify e)
                 (if (= (length e) 1)
                   (car e)
                   (cons 'begin e)))
               (define (Expr* e* env)
                 (map (curryr Expr env) e*))
               (with-output-language (L0 Expr)
                                     (define (expand-let let-e env)
                                       (match let-e
                                         (`(let ,bindings ,body ...)
                                           (let ((env* (foldl
                                                         (lambda (n e) (extend-env e (car n)))
                                                         env
                                                         bindings)))
                                             (define (expand-bindings b)
                                               (match b
                                                 ('() (Expr (maybe-beginify body) env*))
                                                 (`((,x : ,t ,e) . ,rest)
                                                   (unless (or (variable? x) (type? t))
                                                     (error "let binding is invalid" `(,x : ,t)))
                                                   `(let ,(var-name env* x) ,t ,(Expr e env)
                                                      ,(expand-bindings rest)))))
                                             (expand-bindings bindings)))
                                         (else
                                           (error "invalid let form" let-e))))))
             (Expr : * (e env) -> Expr ()
                   (match e
                     ((? constant?) e)
                     ((? variable?) (var-name env e))
                     (`(begin ,exprs ...)
                       (let ((e (Expr* exprs env)))
                         `(begin ,(drop-right e 1) ... ,(last e))))
                     (`(if ,a ,b ,c)
                       `(if ,(Expr a env) ,(Expr b env) ,(Expr c env)))
                     (`(,(or 'lambda 'λ) ,type ,args ,body ...)
                       (when (check-duplicates args)
                         (error "duplicate arg names"))
                       (let ((env* (foldl
                                     (lambda (n e) (extend-env e n))
                                     env
                                     args)))
                         `(λ ,type (,(map (curry var-name env*) args) ...) ,(Expr (maybe-beginify body) env*))))
                     (`(let ,_ ...)
                       (expand-let e env))
                     (`(,f ,args ...)
                       `(,(Expr f env) ,(Expr* args env) ...))))
             (Expr e (hash)))

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
                     (let ((t1 (infer e1 env)) (t2 (infer e2 env)))
                       (unless (equal? t1 t2)
                         (error "if statement paths must return same type, but got" t1 "and" t2))
                       t1))
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
                     (let* ((fty (infer e0 env)) (rargsty (map (curryr infer env) e1))
                                                 (argsty (drop-right (cdr fty) 1)))
                       (unless (equal? argsty rargsty)
                         (error "function has incorrect type - expecting args of type"
                                argsty
                                "but got"
                                rargsty
                                "for"
                                (cons e0 e1)))
                       (last fty))))
             (Expr : Expr (ir env) -> Expr ()
                   ((let ,x ,t ,[e] ,body)
                    `(let ,x ,e
                       ,(Expr body (env-add env x (binding x t)))))
                   ((λ ,t (,x* ...) ,[body])
                    `(λ (,x* ...) ,body)))
             (infer ir (hash '%add (binding '%add '(-> int int int))
                             '%gt (binding '%gt '(-> int int bool))))
             (Expr ir (hash '%add (binding '%add '(-> int int int))
                            '%gt (binding '%gt '(-> int int bool)))))

(define-pass convert-to-anf : L1 (ir) -> L1.1 ()
             (definitions
               (define (value? m)
                 (match m
                   ((? constant?) #t)
                   ((? variable?) #t)
                   (else #f)))
               (with-output-language (L1.1 Expr)
                                     (define (maybe-normalize e gen-body)
                                       (if (value? e)
                                         (gen-body e)
                                         (let ((tmp-var (gensym)))
                                           `(let ,tmp-var ,(Expr e)
                                              ,(gen-body tmp-var)))))
                                     (define (maybe-normalize* e* gen-body)
                                       (match e*
                                         ('() (gen-body '()))
                                         (`(,e . ,rest)
                                           (maybe-normalize e
                                                            (λ (t)
                                                               (maybe-normalize* rest
                                                                                 (λ (t*)
                                                                                    (gen-body (cons t t*)))))))))))
             (Expr : Expr (ir) -> Expr ()
                   ((if ,e0 ,[e1] ,[e2])
                    (maybe-normalize e0
                                     (λ (v)
                                        `(if ,v ,e1 ,e2))))
                   ((,e0 ,e1 ...)
                    (maybe-normalize e0 (λ (t)
                                           (maybe-normalize* e1
                                                             (λ (t*)
                                                                `(,t ,t* ...))))))))

(define-pass lift-lambdas : L1.1 (ir) -> L2 ()
             (definitions
               (define *fs* '())
               (with-output-language (L2 Expr)
                                     (define (make-func! l x* body)
                                       (set! *fs*
                                         (cons `(label ,l (,x* ...) ,body) *fs*)))))
             (Expr : Expr (ir) -> Expr ()
                   ((λ (,x* ...) ,body)
                    (let ((l (gensym 'lambda)))
                      (make-func! l x* (Expr body))
                      `(lref ,l))))
             (let ((e (Expr ir)))
               `(program (label fir_entry () ,e) ,*fs* ...)))

(define-pass emit-asm : L2 (ir) -> * ()
             (definitions
               (define (frame-locals frame)
                 (filter (compose positive? cdr) frame))

               (define (slot-index frame e)
                 (cond ((findf (lambda (x) (eq? (car x) e)) frame) => cdr)
                       (else (error "failed to get index for" e))))

               (define (emit-immediate frame e)
                 (match e
                   ((? variable?)
                    (if (%name? e)
                      (format "$~a" (clean-%name e))
                      (format "~a(%ebp)" (- (* +word-size+ (slot-index frame e))))))
                   ((? number?) (format "$~a" e))
                   ((? boolean?) (format "$~a" (if e 1 0))))))

             (Expr : Expr (ir (frame '())) -> * ()
                   (,x (printf "movl ~a, %eax\n" (emit-immediate frame x)))
                   (,c (printf "movl ~a, %eax\n" (emit-immediate frame c)))
                   ((begin ,e* ... ,e) (begin
                                         (map (curryr Expr frame) e*)
                                         (Expr e frame)))
                   ((if ,e0 ,e1 ,e2)
                    (let ((else-label (gensym 'else)) (end-label (gensym 'end)))
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
                   ((lref ,x)
                    (printf "movl $~a, %eax\n" x))
                   ((,e0 ,e1 ...)
                    (Expr e0 frame)
                    (for-each (lambda (v)
                                (printf "pushl ~a\n" (emit-immediate frame v)))
                              e1)
                    (printf "call *%eax\n"))
                   ((label ,x (,x* ...) ,body)
                    (printf ".global ~a\n" x)
                    (printf ".type ~a @function\n" x)
                    (printf ".align 8\n")
                    (printf "~a:\n" x)
                    (printf "pushl %ebp\n")
                    (printf "movl %esp, %ebp\n")
                    (Expr body
                          (map
                            (lambda (a i)
                              (cons a (- i)))
                            x*
                            (iota (length x*) 2)))
                    (printf "movl %ebp, %esp\n")
                    (printf "popl %ebp\n")
                    (printf "ret\n"))
                   ((program ,e* ...)
                    (for-each (curryr Expr frame) e*)))
             (Expr ir '()))

(define dump? (make-parameter #f))

(define-syntax (maybe-dump stx)
  (syntax-case stx ()
    ((_ unparse transform)
     #'(if (dump?)
         (lambda (f)
           (let ((r (transform f)))
             (println (quote transform))
             (println (unparse r))
             r))
         transform))))

(define (fir-compile l)
  ((compose
     emit-asm
     (maybe-dump unparse-L2   lift-lambdas)
     (maybe-dump unparse-L1.1 convert-to-anf)
     (maybe-dump unparse-L1   type-check-and-discard-type-info)
     (maybe-dump unparse-L0   parse-and-desugar))
   l) '())
