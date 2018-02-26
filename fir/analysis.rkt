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

(type-check-and-discard-type-info (parse-L0 '(let f bool #t (if f 2 2))))
