#lang racket

(require sugar)

(define (label s)
  (list (string-append (->string s) ":")))

(define (anf->assembly expr)
  (match expr
    ((? number? d) `("movl" ,(string-append "$" (->string d)) "%eax"))
    (`(if ,(? symbol? cond-var) ,then-expr ,else-expr)
      (let ((else-label (gensym 'else))
            (end-label (gensym 'end)))
        `(("cmp" "$0" "%ebx")
          ("jz" ,else-label)
          ,(anf->assembly then-expr)
          ("jmp" ,end-label)
          ,(label else-label)
          ,(anf->assembly else-expr)
          ,(label end-label))))
    (`(let ,name ,val ,body)
      ; TODO: bind name
      `((pushl "%ebx")
        (movl ,(string-append "$" (->string val)) "%ebx")
        ,@(anf->assembly body)
        (popl "%ebx")))))

(define (print-asm instruction)
  (let ((instruction* (map ->string instruction)))
   (displayln
     (cond ((= 3 (length instruction*))
            (string-append (car instruction*)
                           " "
                           (string-join (cdr instruction*) ",")))
           ((= 2 (length instruction*)) (string-join instruction* " "))
           (else
             (car instruction*))))))

(define (compile code)
  (displayln ".text")
  (displayln ".global fir_entry")
  (displayln ".type fir_entry @function")
  (displayln ".align 8")
  (displayln "fir_entry:")
  (displayln "movl $3, %eax")
  (map print-asm code)
  (displayln "ret"))

(compile (anf->assembly '(let f 0 (if f 1 2))))
