#lang racket
(require racket/contract)

(struct func (params body env))
(struct stream (first (current #:mutable) upf))
(struct primfunc (f))

(define/contract (lookup env n)
  (-> (listof hash?) symbol? any)
  (match env
         ('() (error "No such binding " n))
         ((cons h r)
          (cond
            ((hash-ref h n #f) => identity)
            (else (lookup r n))))))

; evaluate list and return the result of the last eval
(define (ieval-list li env)
  (cond
    ((null? li) 'nil)
    ((cons? li)
     (let ((r (ieval (car li) env)))
       (if (null? (cdr li))
         r
         (ieval-list (cdr li) env))))))

(define (bindings->frame blist)
  (make-hash blist))

(define/contract (iapply f args)
  (-> (or/c func? primfunc? stream?) list? any)
  (cond
    ((primfunc? f) (apply (primfunc-f f) args))
    ((func? f)
     (let ((fenv (bindings->frame (map cons (func-params f) args))))
       (ieval-list (func-body f) (cons fenv (func-env f)))))
    ((stream? f) (stream-current f))
    (else
      (error "expected a function, but got" f))))

(define/contract (ieval expr env)
  (-> (or/c stream? symbol? number? string? list?) (listof hash?) any)
  (define (ieval-bindings li env)
    (map
      (match-lambda
        ((cons b v)
         (unless (symbol? b)
           (error "expected symbol for binding: " b))
         (cons b (ieval (car v) env)))
        (else
          (error "invalid binding"))) li))
  (match expr
         (`(quote ,e) e)
         (`(let ,(list bindings ...) ,body ...)
           ; desugar into a lambda
           (let ((bindings (ieval-bindings bindings env)))
             (iapply
               (func (map car bindings) body env)
               (map cdr bindings))))
         (`(letrec ,(list bindings ...) ,body ...)
           (let ((fr (bindings->frame (map cons (map car bindings) (make-list (length bindings) 'nil)))))
             (for/list ((b bindings))
                       (hash-set! fr (car b) (ieval (cadr b) (cons fr env))))
             (ieval-list body (cons fr env))))
         (`(lambda ,(list params ...) ,body ...)
           (func params body env))
         (`(.\ ,body ...)
           (func '() body env))
         ((list f args ...)
          (iapply (ieval f env) (map ((curryr ieval) env) args)))
         ((? number? string?) expr)
         ((? symbol?) (lookup env expr))))

(define (ensure! msg pred v)
  (unless (pred v)
    (error (string-append msg ":") v))
  v)

(define (make-default-env)
  (list
    (hash
      '+ (primfunc +)
      '- (primfunc -)
      '* (primfunc *)
      '/ (primfunc /)
      'fby (primfunc
             (lambda (init upf)
               (stream init init upf)))
      'force (primfunc
               (lambda (s)
                 (ensure! "should be a stream" stream? s)
                 (stream-current s)))
      'first (primfunc
               (lambda (s)
                 (ensure! "should be a stream" stream? s)
                 (stream-first s)))
      'next! (primfunc
               (lambda (s)
                 (ensure! "should be a stream" stream? s)
                 (set-stream-current! s (iapply (stream-upf s) '()))
                 (stream-current s)))
      'finite-stream (primfunc
                       ; this - is just a little horrible.
                       (lambda args
                         (ensure! "need at least one element in stream" (compose not zero? length) args)
                         (stream (car args) (car args)
                                 (primfunc
                                   (lambda ()
                                     (cond
                                       ((null? (cdr args)) 'nil)
                                       (else
                                         (set! args (cdr args))
                                         (car args))))))))
      )))

(ieval '(letrec ((input (finite-stream 3 3 3 3 3))
                 (sum (fby (first input) (.\ (+ (sum) (next! input)))))
                 (n (fby 1 (.\ (+ (n) 1)))))
          (/ (sum) (n)))
       (make-default-env))
