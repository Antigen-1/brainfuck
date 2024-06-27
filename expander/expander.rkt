#lang racket/base
(require syntax/parse/define
         (prefix-in o: "data.rkt")
         (for-syntax racket/base syntax/parse/define racket/list racket/match)
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
(define-syntax-parser n:begin
  #:literals (n:begin)
  ((_ (n:begin ist ...) pst ...) #'(n:begin ist ... pst ...))
  ((_ step (n:begin) ...) #'step)

  ((_ st0 st1 st ...) #'(begin st0 (n:begin st1 st ...)))
  ((_) #'(begin (void))))
(define-syntax-parser loop
  ((_ step ...)
   (let ((ex (local-expand #'(n:begin step ...) 'expression (list #'n:begin))))
     (if (null? (cdr (syntax-e ex)))
         #`(if (zero? (o:cur))
               (void)
               (let lp () (lp)))
         #`(if (zero? (o:cur))
               (void)
               (let lp ()
                 #,ex
                 (if (zero? (o:cur)) (void) (lp))))))))
(define-syntax-parser n:#%module-begin
  ((_ program)
   #`(#%module-begin
      (let ()
        #,((compose1
            optimize
            merge-operators
            flatten-program)
           #'program)))))
;; Optimized interposition points
(define-syntax-parse-rule (reset)
  (o:cur 0))
(define-syntax-parser loop/counter
  ((_ 0 (body ...))
   #'(if (zero? (o:cur))
         (void)
         (let lp ()
           (n:begin body ...)
           (lp))))
  ;; This offset can be negative
  ((_ offset (body ...))
   #`(let ((v (o:cur)))
       (if (zero? v)
           (void)
           (let lp ((v v))
             (let ((nv (#,(o:dispatch-+ #'offset) v offset)))
               (n:begin body ...)
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
  (define-syntax-class shift
    #:description "shiftl or shiftr"
    #:literals (shiftl shiftr)
    (pattern (shiftl . n:count) #:with offset #`#,(- (syntax->datum #'n)))
    (pattern (shiftr . n:count) #:with offset #'n))
  (define-splicing-syntax-class reset-loops
    #:description "loops that simply reset the current value (like [[-]][-])"
    #:literals (loop)
    (pattern (~seq (loop _:add/sub)))
    (pattern (~seq (loop _:reset-loops) ...+)))
  (define-splicing-syntax-class reset-loop/add/sub-seq
    #:description "reset-loop/add/sub sequence"
    (pattern (~seq _:reset-loops _:reset-loop/add/sub-seq))
    (pattern (~seq _:add/sub _:reset-loop/add/sub-seq))
    (pattern (~seq)))
  (define-syntax-class element->seq-helper
    #:description "element->seq-helper"
    #:literals (n:begin)
    (pattern (n:begin st ...) #:with seq #'(st ...))
    (pattern sst #:with seq #'(sst)))

  (define-for-syntax (make-id stx cls (base "_"))
    (datum->syntax
     stx
     (string->symbol
      (string-append
       base ":"
       (symbol->string (syntax->datum cls))))))

  (define-syntax (cls->pred stx)
    (syntax-parse stx
      ((_ cls)
       (with-syntax ((id (make-id #'stx #'cls)))
         #'(lambda (stx)
             (syntax-parse stx
               (id #t)
               (_ #f)))))
      ((_ #:splicing cls)
       (with-syntax ((id (make-id #'stx #'cls)))
         #'(lambda (stx)
             (syntax-parse stx
               ((id) #t)
               (_ #f)))))))
  (define-syntax (get-offset stx)
    (syntax-parse stx
      ((_ cls s)
       (with-syntax ((id (make-id #'stx #'cls "op")))
         #'(syntax-parse s
             (id (syntax->datum #'op.offset)))))))

  (define (shift? stx)
    ((cls->pred shift) stx))
  (define (add/sub? stx)
    ((cls->pred add/sub) stx))
  (define (reset-loop? stx)
    ((cls->pred #:splicing reset-loops) #`(#,stx)))

  ;; (->* ((listof syntax?))
  ;;      (exact-nonnegative-integer? (listof syntax?) (listof (listof syntax?)) (listof syntax?))
  ;;      (values exact-nonnegative-integer? (listof (listof syntax?)) (listof syntax?)))
  (define (split-sequence/closure sts (offset 0) (current null) (blocks null) (updates null))
    (if (null? sts)
        (values offset
                (reverse (if (null? current) blocks (cons (reverse current) blocks)))
                (reverse updates))
        (syntax-parse (car sts)
          (st:shift
           (define no (+ offset (get-offset shift #'st)))
           (define merge? (and (zero? no) (not (null? current))))
           (split-sequence/closure
            (cdr sts)
            no
            (if merge? null (cons #'st current))
            (if merge?
                (cons (reverse (cons #'st current)) blocks)
                blocks)
            updates))
          (st
           (define merge? (not (zero? offset)))
           (split-sequence/closure
            (cdr sts)
            offset
            (if merge? (cons #'st current) current)
            blocks
            (if (not merge?) (cons #'st updates) updates))))))
  ;; (-> syntax? (values (cons/c (or/c 'rel 'abs) exact-integer?)
  ;;                     (listof (listof syntax?))
  ;;                     (listof (listof syntax?))))
  (define (get/rest-closure-suffix stx-list)
    (let*-values (((rest blocks updates) (split-sequence/closure (syntax->list stx-list)))
                  ((r) (foldl (lambda (st cnt)
                                (if (reset-loop? st)
                                    '(abs . 0)
                                    (cons (car cnt) (+ (cdr cnt) (get-offset add/sub st)))))
                              '(rel . 0) updates))
                  ((closure suffix)
                   (split-at-right blocks (if (zero? rest) 0 1))))
      (values r closure suffix)))
  ;; (-> syntax? (listof (cons/c (or/c 'reorder 'no-reorder) (non-empty-listof syntax?))))
  (define (split-sequence/reordering stx-list)
    (define (make-block type seq)
      (cons type seq))
    (call-with-values
     (lambda ()
       (for/fold ((results null)
                  (current null))
                 ((st (in-list (syntax->list stx-list))))
         (if (begin-reorder? #`(#,@(reverse (cons st current))))
             (values results (cons st current))
             (if (null? current)
                 (values (cons (make-block 'no-reorder (list st)) results) null)
                 (values (cons (make-block 'no-reorder (list st))
                               (cons (make-block 'reorder (reverse current)) results))
                         null)))))
     (lambda (rs ct)
       (if (null? ct)
           (reverse rs)
           (reverse (cons (make-block 'reorder (reverse ct)) rs))))))

  (define (begin-reorder? stx-list)
    (define lst (syntax->list stx-list))

    ;; range: (cons/c exact-integer? exact-integer?)
    (define (make-range p v)
      (cons (min (car p) v)
            (max (cdr p) v)))
    (define (merge-range p1 p2)
      (cons (min (car p1) (car p2))
            (max (cdr p1) (cdr p2))))
    (define (in-range? rng v)
      (> (* (+ (car rng) v) (+ (cdr rng) v)) 0))

    ;; (or/c #f (cons/c <range> exact-integer?))
    (and (pair? lst)
         (let-values (((rest blocks updates) (split-sequence/closure lst)))
           (and (andmap (lambda (st) (or (add/sub? st) (reset-loop? st)))
                        updates)
                (foldl (lambda (st old)
                         (and old
                              (or (and (add/sub? st) old)
                                  (syntax-parse st
                                    #:literals (loop)
                                    ((loop s ...)
                                     (define r (begin-reorder? #'(s ...)))
                                     (and r
                                          (zero? (cdr r))
                                          (in-range? (car r) (cdr old))
                                          (cons (merge-range
                                                 (car old)
                                                 (cons (+ (cdr old) (caar r)) (+ (cdr old) (cdar r))))
                                                (cdr old))))
                                    (op:shift
                                     (define n-offset (+ (cdr old) (get-offset shift #'op)))
                                     (cons (make-range (car old) n-offset) n-offset))
                                    (_ #f)))))
                       (cons (cons 0 0) 0)
                       (apply append blocks))))))
  (define (loop-counter? stx-list)
    (let ((r (begin-reorder? stx-list)))
      (and r (zero? (cdr r)))))

  (define reorder? (make-parameter #t))

  (define-splicing-syntax-class optimizer
    #:description "optimizer"
    #:literals (loop n:begin add sub shiftl shiftr read put)
    ;; Note: The order of optimizers matters
    ;; Dispatcher
    ;; Reordering is enabled for sequences
    (pattern (~seq st:element->seq-helper ...+)
             #:when (reorder?)
             #:with optimized
             (let ((blocks (split-sequence/reordering
                            #`(#,@(apply append (map syntax->list (syntax->list #'(st.seq ...))))))))
               ((lambda (ll) #`((n:begin #,@(apply append (map syntax->list ll)))))
                (for/list ((b (in-list blocks)))
                  (if (eq? (car b) 'reorder)
                      (let*-values (((r closure suffix) (get/rest-closure-suffix #`(#,@(cdr b))))
                                    ((stop-reorder?) (and (= (length blocks) 1)
                                                          (or (null? closure)
                                                              (null? suffix)))))
                        ;; Avoid infinite loops
                        (parameterize ((reorder? (not stop-reorder?)))
                          #`((n:begin
                              #,@(match r
                                   ((cons 'rel 0) #'())
                                   ((cons 'abs n) #`((o:cur (#,(o:dispatch-and #`#,n) #,n 255))))
                                   ((cons 'rel n)
                                    #`((o:cur (#,(o:dispatch-+ #`#,n) (o:cur) #,n)))))
                              #,((compose1 optimize merge-operators)
                                 #`(n:begin #,@(apply append closure)))
                              #,(optimize #`(n:begin #,@(apply append suffix)))))))
                      ;; Avoid infinite loops
                      (parameterize ((reorder? (not (= (length blocks) 1))))
                        #`(#,(optimize #`(n:begin #,@(cdr b))))))))))

    ;; Resetting
    (pattern (~seq _:reset-loop/add/sub-seq
                   _:reset-loops
                   post:add/sub ...)
             #:with optimized
             (if (null? (syntax->datum #'(post ...)))
                 #'((reset))
                 #`(#,(merge-operators #'(n:begin post ...)))))
    ;; Counter
    (pattern (~seq (loop st ...))
             #:when (loop-counter? #'(st ...))
             #:with optimized
             (let*-values (((r closure _) (get/rest-closure-suffix #'(st ...)))
                           ((body) ((compose1 optimize merge-operators)
                                    #`(n:begin (n:begin #,@(apply append closure))))))
               (match r
                 ((cons 'rel n)
                  #`((loop/counter
                      #,n
                      (#,((compose1 optimize merge-operators) #`(n:begin #,@(apply append closure)))))))
                 ((cons 'abs 0)
                  #`((loop/once st ...)))
                 ((cons 'abs n)
                  #`((loop (o:cur (#,(o:dispatch-and #`#,n) #,n 255)) st ...))))))
    ;; The body of ordinary loops
    (pattern (~seq (loop st ...))
             #:with optimized #`((loop #,(optimize #'(n:begin st ...)))))))
(define-for-syntax (optimize stx)
  (syntax-parse stx
    #:literals (loop n:begin)
    ((loop) #'(loop))
    ((n:begin) #'(n:begin))
    ((n:begin step0:optimizer step ...)
     #`(n:begin #,@#'step0.optimized #,(parameterize ((reorder? #t)) (optimize #'(n:begin step ...)))))
    ((loop step0:optimizer step ...)
     #`(loop #,@#'step0.optimized #,(parameterize ((reorder? #t)) (optimize #'(n:begin step ...)))))
    (ot #'ot)))
