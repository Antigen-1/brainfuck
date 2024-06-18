#lang racket/base
(require (submod racket/performance-hint begin-encourage-inline))
(provide add sub shiftr shiftl read put cur)

;; former current latter
;; (vector/c list? byte? list?)
(define main-vector (vector null 0 null))

(define-syntax-rule (make-mutator-and-accessor vec ind)
  (case-lambda
    (() (vector-ref vec ind))
    ((v) (vector-set! vec ind v))))

(begin-encourage-inline
  (define cur (make-mutator-and-accessor main-vector 1))
  (define former (make-mutator-and-accessor main-vector 0))
  (define latter (make-mutator-and-accessor main-vector 2))

  ;; n: exact-positive-integer?
  (define (add n)
    (cur (bitwise-and (+ n (cur)) 255)))
  (define (sub n)
    (cur (bitwise-and (- (cur) n) 255)))
  (define (shiftr n (f (former)) (c (cur)) (l (latter)))
    (if (zero? n)
        (begin (former f)
               (cur c)
               (latter l))
        (if (null? f)
            (shiftr (sub1 n) null 0 (cons c l))
            (shiftr (sub1 n) (cdr f) (car f) (cons c l)))))
  (define (shiftl n (f (former)) (c (cur)) (l (latter)))
    (if (zero? n)
        (begin (former f)
               (cur c)
               (latter l))
        (if (null? l)
            (shiftl (sub1 n) (cons c f) 0 null)
            (shiftl (sub1 n) (cons c f) (car l) (cdr l)))))
  (define (read n)
    (cur
     (for/last ((_ (in-range n)))
       (read-byte))))
  (define (put n)
    (define bt (cur))
    (for ((_ (in-range n)))
      (write-byte bt))))
