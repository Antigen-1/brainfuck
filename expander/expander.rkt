#lang racket/base
(require syntax/parse/define
         (prefix-in o: "data.rkt")
         (for-syntax racket/base))
(provide program loopstart loopend slotop ptrop unit
         (rename-out (n:#%module-begin #%module-begin)))

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
(define-syntax-parse-rule (n:begin step ...) (begin step ...))
(define-syntax-parse-rule (loop step ...)
  (let lp ()
    step ...
    (if (zero? (o:cur))
        (void)
        (lp))))
(define-syntax-parser n:#%module-begin
  ((_ program)
   #`(#%module-begin
      (let ()
        #,((compose1 optimize merge-operators flatten-program) #'program)))))

;; Hooks
(define-syntax (program stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (loopstart stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (loopend stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (slotop stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (ptrop stx) (raise-syntax-error #f "Used outside the expander" stx))
(define-syntax (unit stx) (raise-syntax-error #f "Used outside the expander" stx))

;; Remove program, unit, loopstart and loopend
;; Introduce n:begin and loop
(define-for-syntax (flatten-program stx)
  (syntax-parse stx
    #:literals (program loopstart loopend unit)
    [(program u ...)
     #`(n:begin #,@(map (lambda (u) (flatten-program u)) (syntax->list #'(u ...))))]
    [(unit (loopstart _) ot ... (loopend _))
     #`(loop #,@(map flatten-program (syntax->list #'(ot ...))))]
    [(unit ot ...)
     #`(n:begin #,@(map flatten-program (syntax->list #'(ot ...))))]
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
(define-for-syntax (merge-operators stx (current #f) (count 0) (result null))
  (define (merge cur cnt r)
    (if (< cnt 0)
        (cons (cons (get-opp cur) (- cnt)) r)
        (if (zero? cnt)
            r
            (cons (cons cur cnt) r))))
  (syntax-parse stx
    #:literals (n:begin loop)
    [(n:begin step1:operator step ...)
     #:when (not current)
     (merge-operators
      #'(n:begin step ...)
      #'step1.operator
      1
      result)]
    [(n:begin step1:operator step ...)
     #:when (operator-type=? current #'step1.operator)
     (merge-operators
      #'(n:begin step ...)
      current
      ((if (opp? current #'step1.operator) sub1 add1) count)
      result)]
    [(n:begin step1:operator step ...)
     (merge-operators
      #'(n:begin step ...)
      #'step1.operator
      1
      (merge current count result))]

    [(n:begin step1 step ...)
     (syntax-parse #'step1
       #:literals (n:begin loop)
       ((n:begin sstep ...)
        (merge-operators #'(n:begin sstep ... step ...)
                         current count result))
       ((loop sstep ...)
        (merge-operators
         #'(n:begin step ...)
         #f
         0
         (cons (merge-operators #'(loop sstep ...) #f 0 null)
               (if current (merge current count result) result)))))]

    [(n:begin)
     #:when (not current)
     #`(n:begin #,@(reverse result))]
    [(n:begin)
     #`(n:begin #,@(reverse (merge current count result)))]

    [(loop step ...)
     #`(loop #,@(cdr (syntax-e (merge-operators #'(n:begin step ...) #f 0 null))))]))

;; The optimizer
(begin-for-syntax
  (define-syntax-class add/sub
    #:description "add/sub"
    #:literals (add sub)
    (pattern (add . _))
    (pattern (sub . _)))
  (define-splicing-syntax-class maybe-add/sub
    #:description "maybe-add/sub"
    (pattern (~seq add/sub:add/sub))
    (pattern (~seq)))

  (define-splicing-syntax-class optimizer
    #:description "optimizer"
    #:literals (loop n:begin add sub shiftl shiftr read put)
    (pattern (~seq _:maybe-add/sub
                   (loop _:add/sub)
                   post:maybe-add/sub)
             #:with optimized
             (if (null? (syntax->datum #'post))
                 #'((o:cur 0))
                 #'post))

    ;; Fallback
    (pattern (~seq st)
             #:with optimized #`(#,(optimize #'st)))))
(define-for-syntax (optimize stx)
  (syntax-parse stx
    #:literals (loop n:begin)
    ((n:begin step0:optimizer step ...)
     #`(n:begin #,@#'step0.optimized #,(optimize #'(n:begin step ...))))
    ((loop step0:optimizer step ...)
     #`(loop #,@#'step0.optimized #,(optimize #'(n:begin step ...))))
    ((loop) #'(if (zero? (o:cur)) (void) (let loop () (loop))))
    ((n:begin) #'(n:begin))
    (ot #'ot)))
