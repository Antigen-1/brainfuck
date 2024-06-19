#lang racket/base
(require syntax/parse/define
         (prefix-in o: "data.rkt")
         (for-syntax racket/base syntax/parse/define)
         (for-meta 2 racket/base))
(provide program loopstart loopend slotop ptrop unit
         (rename-out (n:#%module-begin #%module-begin)))

(begin-for-syntax
  (define-syntax-class
    count
    #:description "count"
    (pattern n #:when (exact-positive-integer? (syntax->datum #'n)))))

;; Interposition points
(define-syntax-parse-rule (add . n:count)
  (o:add n))
(define-syntax-parse-rule (sub . n:count)
  (o:sub n))
(define-syntax-parse-rule (read . n:count)
  (o:read n))
(define-syntax-parse-rule (put . n:count)
  (o:put n))
(define-syntax-parse-rule (shiftr . n:count)
  (o:shiftr n))
(define-syntax-parse-rule (shiftl . n:count)
  (o:shiftl n))
(define-syntax-parse-rule (n:begin step ...) (begin step ...))
(define-syntax-parse-rule (loop step ...)
  (if (zero? (o:cur))
      (void)
      (let lp ()
        step ...
        (if (zero? (o:cur)) (void) (lp)))))
(define-syntax-parser n:#%module-begin
  ((_ program)
   #`(#%module-begin
      (let ()
        #,((compose1 optimize merge-operators flatten-program) #'program)))))
;; Optimized Interposition points
(define-syntax-parse-rule (reset)
  (o:cur 0))
(define-syntax-parser loop/counter
  ((_ 0 (body ...))
   #'(if (zero? (o:cur))
         (void)
         (let lp ()
           body ...
           (lp))))
  ;; This offset can be negative
  ((_ offset (body ...))
   #'(let ((v (o:cur)))
       (if (zero? v)
           (void)
           (let lp ((v v))
             (let ((nv (+ v offset)))
               body ...
               (if (zero? nv)
                   (reset)
                   (lp nv))))))))
(define-syntax-parse-rule (loop/once body ...)
  (if (zero? (o:cur))
      (void)
      (n:begin body ... (reset))))

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
             #:with cnt #'1
             #:with operator
             (case (syntax->datum #'op)
               ((+) #'add)
               ((-) #'sub)
               ((\,) #'read)
               ((\.) #'put)))
    (pattern (ptrop op)
             #:with cnt #'1
             #:with operator
             (case (syntax->datum #'op)
               ((>) #'shiftr)
               ((<) #'shiftl)))
    ;; Used in the optimizer
    (pattern (op . n:count)
             #:with cnt #'n
             #:with operator #'op)))
;; Remove slotop and ptrop
;; Introduce add, sub, read, put, shiftr and shiftl
;; The optimizer also uses it to merge introduced operators
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
      (syntax->datum #'step1.cnt)
      result)]
    [(n:begin step1:operator step ...)
     #:when (operator-type=? current #'step1.operator)
     (merge-operators
      #'(n:begin step ...)
      current
      ((if (opp? current #'step1.operator) - +) count (syntax->datum #'step1.cnt))
      result)]
    [(n:begin step1:operator step ...)
     (merge-operators
      #'(n:begin step ...)
      #'step1.operator
      (syntax->datum #'step1.cnt)
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
    (pattern (add . n:count) #:with offset #'n)
    (pattern (sub . n:count) #:with offset #`#,(- (syntax->datum #'n))))
  (define-splicing-syntax-class maybe-add/sub
    #:description "maybe-add/sub"
    (pattern (~seq add/sub:add/sub))
    (pattern (~seq)))
  (define-syntax-class shift
    #:description "shiftl or shiftr"
    #:literals (shiftl shiftr)
    (pattern (shiftl . n:count) #:with offset #`#,(- (syntax->datum #'n)))
    (pattern (shiftr . n:count) #:with offset #'n))
  (define-syntax-class pure
    #:description "steps without IO"
    (pattern _:add/sub)
    (pattern _:shift))
  (define-syntax-class loop-without-shift
    #:description "loops without shifts (like [-[+]])"
    #:literals (loop)
    (pattern (loop (~or _:add/sub _:loop-without-shift) ...)))
  (define-syntax-class reset-loop
    #:description "loops that simply reset the current value (like [[-]])"
    #:literals (loop)
    (pattern (loop (~or _:add/sub _:reset-loop))))

  (define-syntax (cls->pred stx)
    (syntax-parse stx
      ((_ cls)
       (with-syntax ((id (datum->syntax
                          #'stx
                          (string->symbol
                           (string-append
                            "_:"
                            (symbol->string (syntax->datum #'cls)))))))
         #'(lambda (stx)
             (syntax-parse stx
               (id #t)
               (_ #f)))))))
  (define-syntax (get-offset stx)
    (syntax-parse stx
      ((_ cls s)
       (with-syntax ((id (datum->syntax
                          #'stx
                          (string->symbol
                           (string-append
                            "op:"
                            (symbol->string (syntax->datum #'cls)))))))
         #'(syntax-parse s
             (id (syntax->datum #'op.offset)))))))

  (define (shift? stx)
    ((cls->pred shift) stx))
  (define (add/sub? stx)
    ((cls->pred add/sub) stx))
  (define (pure? stx)
    ((cls->pred pure) stx))
  (define (loop-without-shift? stx)
    ((cls->pred loop-without-shift) stx))

  ;; Used in the counter optimizer
  (define (split-loop-body/counter sts (offset 0) (current null) (blocks null) (updates null))
    (if (null? sts)
        (values offset (reverse (cons (reverse current) blocks)) (reverse updates))
        (syntax-parse (car sts)
          (st:shift
           (define no (+ offset (get-offset shift #'st)))
           (define merge? (and (zero? no) (not (null? current))))
           (split-loop-body/counter
            (cdr sts)
            (if merge? 0 no)
            (if merge? null (cons #'st current))
            (if merge?
                (cons (reverse (cons #'st current)) blocks)
                blocks)
            updates))
          (st
           (define merge? (not (zero? offset)))
           (split-loop-body/counter
            (cdr sts)
            offset
            (if merge? (cons #'st current) current)
            blocks
            (if (not merge?) (cons #'st updates) updates))))))

  (define-splicing-syntax-class optimizer
    #:description "optimizer"
    #:literals (loop n:begin add sub shiftl shiftr read put)
    (pattern (~seq _:maybe-add/sub
                   _:reset-loop
                   post:maybe-add/sub)
             #:with optimized
             (if (null? (syntax->datum #'post))
                 #'((reset))
                 #'post))
    (pattern (~seq (loop st ...))
             #:when (let-values (((rest blocks updates) (split-loop-body/counter (syntax->list #'(st ...)))))
                      (and (zero? rest)
                           (andmap (lambda (st) (or (pure? st) (loop-without-shift? st)))
                                   (apply append blocks))
                           (andmap add/sub? updates)))
             #:with optimized
             (let*-values (((_ blocks updates) (split-loop-body/counter (syntax->list #'(st ...))))
                           ((r) (foldl (lambda (st cnt) (+ cnt (get-offset add/sub st))) 0 updates)))
               #`((loop/counter
                   #,r
                   (#,((compose1 optimize merge-operators)
                       #`(n:begin #,@(apply append blocks))))))))
    (pattern (~seq (loop st ... _:reset-loop))
             #:with optimized #'((loop/once st ...)))

    ;; Fallback
    (pattern (~seq (loop st ...))
             #:with optimized #`((loop #,(optimize #'(n:begin st ...)))))
    ;; Actually this branch will never be reached because `merge-operators` won't insert `n:begin` in the body of `n:begin`
    (pattern (~seq (n:begin st ...) ot ...)
             #:with optimized #`(#,((compose1 optimize merge-operators) #'(n:begin st ... ot ...))))
    (pattern (~seq st)
             #:with optimized #`(#,(optimize #'st)))))
(define-for-syntax (optimize stx)
  (syntax-parse stx
    #:literals (loop n:begin)
    ((loop) #'(if (zero? (o:cur)) (void) (let loop () (loop))))
    ((n:begin) #'(n:begin))
    ((n:begin step0:optimizer step ...)
     #`(n:begin #,@#'step0.optimized #,(optimize #'(n:begin step ...))))
    ((loop step0:optimizer step ...)
     #`(loop #,@#'step0.optimized #,(optimize #'(n:begin step ...))))
    (ot #'ot)))
