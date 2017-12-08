#lang racket
;;; notes ;;;
; lazy evaluation is absolutely necessary

(require racket/contract)
(require sugar)
(require graph)
(require (only-in srfi/1 find))

(define (prompt-for-value name)
  (string-append "Enter value for " (symbol->string name) ": ")
  (read))

(define *graph* (unweighted-graph/directed '()))
(define-vertex-property *graph* node-f #:init (prompt-for-value $v))

(define (internal-def? k)
  (hash-has-key? *internal-funcs* k))

(define (add-uniq-vertex-with-f! e f)
  (let ((n (gensym (string-append (->string e) "-"))))
    (add-vertex! *graph* n)
    (node-f-set! n f)
    n))

(define (compile-expr expr)
  (match expr
    (`(define ,var ,expr)
      (add-vertex! *graph* var)
      (add-directed-edge! *graph* (compile-expr expr) var)
      (node-f-set! var identity)
      var)
    (`(fby ,f ,u)
      (let* ((first (compile-expr f))
             (upf (compile-expr u))
             (cur first)
             (fby-node
               (add-uniq-vertex-with-f! 'fby
                                        ; FIXME: this is wrong too
                                        (lambda (cmd)
                                          (set! cur (compile-expr upf))
                                          ))))
        (add-directed-edge! *graph* first fby-node)
        (add-directed-edge! *graph* fby-node upf)
        (add-directed-edge! *graph* upf fby-node)
        fby-node
        ))
    ((list f args ...)
     (let ((f (compile-expr f)))
       (map (λ (e) (add-directed-edge! *graph* e f))
            (map compile-expr args))
       f))
    ((? internal-def?)
     (add-uniq-vertex-with-f! expr (hash-ref *internal-funcs* expr)))
    ((? number? string?)
     (add-vertex! *graph* expr)
     (node-f-set! expr (lambda () expr))
     expr)
    ((? symbol?)
     (cond
       ((node-f expr #:default #f) => (lambda (_) expr))
       (else (add-vertex! *graph* expr)
             expr)))
    (else (error "Unknown expression"))))

(define (parents g v)
  (cond
    ((filter (λ (e) (eq? (cadr e) v)) (get-edges g)) => (curry map car))
    (else '())))

(define (evaluate g v)
  (let ((args (map (curry evaluate g) (parents g v))))
    (apply (node-f v) args)))

(define *internal-funcs*
  (hash
    '+ +
    '- -
    '* *
    '/ /))

;(compile-expr '(+ (- 1 2) 3))
;(compile-expr '(define total (fby 0 (+ total x))))
(compile-expr '(define a 1))


(evaluate *graph* 'a)

(let* ((graphviz-str (graphviz *graph*))
       (escaped-graphviz (string-replace graphviz-str "\"" "\\\""))
       (cmd (string-append "printf \"" escaped-graphviz "\" | dot -Tpng | feh -")))
  (display escaped-graphviz)
  (system cmd))
