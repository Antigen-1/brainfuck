#lang racket/base
(require syntax/parse/define
         (prefix-in o: "data.rkt")
         (prefix-in r: racket/base)
         (for-syntax racket/base))
(provide program loopstart loopend slotop ptrop unit
         #%module-begin)

;; Interposition points
(define-syntax-parse-rule (add . n:integer)
  (o:add n))
(define-syntax-parse-rule (sub . n:integer)
  (o:sub n))
(define-syntax-parse-rule (read . n:integer)
  (o:read n))
(define-syntax-parse-rule (put . n:integer)
  (o:put n))
(define-syntax-parse-rule (shiftr . n:integer)
  (o:shiftr n))
(define-syntax-parse-rule (shiftl . n:integer)
  (o:shiftl n))
(define-syntax-parse-rule (begin step ...) (let () step ...))
(define-syntax-parse-rule (loop step ...)
  (let lp ()
    step ...
    (if (zero? (o:cur))
        (void)
        (lp))))
(define-syntax-parser #%module-begin
  ((_ program)
   #`(r:#%module-begin #,(merge-operators (flatten-program #'program)))))

;; Hooks
(define-syntax (program stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (loopstart stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (loopend stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (slotop stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (ptrop stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (unit stx) (raise-syntax-error #f "Used outside the expander" stx))

;; Remove program, unit, loopstart and loopend
;; Introduce begin and loop
(define-for-syntax (flatten-program stx)
  (syntax-parse stx
    #:literals (program loopstart loopend unit)
    [(program u ...)
     (cons #'begin (map (lambda (u) (flatten-program u)) (syntax->list #'(u ...))))]
    [(unit (loopstart _) ot ... (loopend _))
     (cons #'loop (map flatten-program (syntax->list #'(ot ...))))]
    [(unit ot ...)
     (cons #'begin (map flatten-program (syntax->list #'(ot ...))))]
    [ot #'ot]))

(define-for-syntax (operator=? op1 op2)
  (case (list (syntax->datum op1) (syntax->datum op2))
    (((+ +) (+ -) (- +) (- -))
     #t)
    (((> >) (> <) (< >) (< <))
     #t)
    (((\, \,) (\. \.))
     #t)
    (else #f)))
(begin-for-syntax
  (define-syntax-class operator
    #:description "brainfuck operator"
    #:literals (slotop ptrop)
    (pattern (slotop op)
             #:with operator
             (case (syntax->datum #'op)
               ((+) #'add)
               ((-) #'sub)
               ((\,) #'read)
               ((\.) #'put)))
    (pattern (ptrop op)
             #:with operator
             (case (syntax->datum #'op)
               ((>) #'shiftr)
               ((<) #'shiftl)))))
;; Remove slotop and ptrop
;; Introduce add, sub, read, put, shiftr and shiftl
(define-for-syntax (merge-operators stx (current #f) (result null))
  (syntax-parse stx
    #:literals (begin loop)
    [(begin step1:operator step ...)
     (not current)
     (merge-operators
      #'(begin step ...)
      (cons #'step1.operator 1)
      result)]
    [(begin step1:operator step ...)
     (operator=? (car current) #'step1.operator)
     (merge-operators
      #'(begin step ...)
      (cons (car current) (add1 (cdr current)))
      result)]
    [(begin step1:operator step ...)
     (merge-operators
      #'(begin step ...)
      (cons #'step1.operator 1)
      (cons current result))]
    [(begin step1 step ...)
     (syntax-parse #'step1
       #:literals (begin loop)
       ((begin sstep ...)
        (merge-operators #'(begin sstep ... step ...)
                         current result))
       ((loop sstep ...)
        (merge-operators
         #'(begin step ...)
         #f
         (cons (merge-operators #'(loop sstep ...) #f null)
               (cons current result)))))]
    [(begin)
     (cons #'begin (reverse (cons current result)))]
    [(loop step ...)
     (cons #'loop (cdr (merge-operators #'(begin step ...) #f null)))]))
