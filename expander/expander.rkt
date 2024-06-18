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
(define-syntax-parse-rule (begin step ...) (let () step ... (void)))
(define-syntax-parse-rule (loop step ...)
  (let lp ()
    step ...
    (if (zero? (o:cur))
        (void)
        (lp))))
(define-syntax-parser #%module-begin
  ((_ program)
   #`(r:#%module-begin #,((compose1 optimize merge-operators flatten-program) #'program))))

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

(define-for-syntax (operator-type=? op1 op2)
  (case (list (syntax->datum op1) (syntax->datum op2))
    (((add add) (add sub) (sub add) (sub sub))
     #t)
    (((shiftl shiftr) (shiftr shiftl) (shiftl shiftl) (shiftr shiftr))
     #t)
    (((read read) (put put))
     #t)
    (else #f)))
(define-for-syntax (get-opp op)
  (case (syntax->datum op)
    ((add) #'sub)
    ((sub) #'add)
    ((shiftl) #'shiftr)
    ((shiftr) #'shiftl)))
(define-for-syntax (identifier=? id1 id2)
  (eq? (syntax->datum id1) (syntax->datum id2)))
(define-for-syntax (opp? op1 op2)
  (and (operator-type=? op1 op2)
       (not (identifier=? op1 op2))))
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
  (define (merge v r)
    (let ((op (car current))
          (cnt (cdr current)))
      (if (< cnt 0)
          (cons (cons (get-opp op) (- cnt))
                r)
          (if (zero? cnt)
              r
              (cons v r)))))
  (syntax-parse stx
    #:literals (begin loop)
    [(begin step1:operator step ...)
     #:when (not current)
     (merge-operators
      #'(begin step ...)
      (cons #'step1.operator 1)
      result)]
    [(begin step1:operator step ...)
     #:when (operator-type=? (car current) #'step1.operator)
     (merge-operators
      #'(begin step ...)
      (cons (car current) ((if (opp? (car current) #'step1.operator) sub1 add1) (cdr current)))
      result)]
    [(begin step1:operator step ...)
     (merge-operators
      #'(begin step ...)
      (cons #'step1.operator 1)
      (merge current result))]

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
               (if current (merge current result) result)))))]

    [(begin)
     #:when (not current)
     (cons #'begin (reverse result))]
    [(begin)
     (cons #'begin (reverse (merge current result)))]

    [(loop step ...)
     (cons #'loop (cdr (merge-operators #'(begin step ...) #f null)))]))

;; The optimizer
(begin-for-syntax
  (define-splicing-syntax-class optimizer
    #:description "optimizer"
    #:literals (loop begin add sub shiftl shiftr read put)
    (pattern (~seq (loop (~or (add . n) (sub . n))))
             #:with optimized #'(#%app o:cur 0))))
(define-for-syntax (optimize stx)
  (syntax-parse stx
    #:literals (loop begin)
    ((begin step0:optimizer step ...)
     #`(begin step0.optimized #,(optimize #'(begin step ...))))
    ((begin step0 step ...)
     #`(begin step0 #,(optimize #'(begin step ...))))
    ((loop step0:optimizer step ...)
     #`(loop step0.optimized #,(optimize #'(begin step ...))))
    ((loop step0 step ...)
     #`(loop step0 #,(optimize #'(begin step ...))))
    ((loop) #'(if (zero? (o:cur)) (void) (let loop () (loop))))
    ((begin) #'(begin))))
