#lang racket/base
(require (submod racket/performance-hint begin-encourage-inline)
         racket/unsafe/ops)
(provide add sub shiftr shiftl read put cur n:+)

;; former current latter
;; (vector/c list? byte? list?)
(define main-vector (vector null 0 null))
(define-syntax-rule (make-observer ind)
  (case-lambda
    (() (unsafe-vector-ref main-vector ind))
    ((v) (unsafe-vector-set! main-vector ind v))))

(begin-encourage-inline
  (define cur (make-observer 1))
  (define former (make-observer 0))
  (define latter (make-observer 2))

  (define (n:+ v1 v2)
   (bitwise-and (+ v1 v2) 255))

  ;; n: exact-positive-integer?
  (define (add n)
    (cur (n:+ (cur) n)))
  (define (sub n)
    (cur (n:+ (cur) (- n))))
  (define (shiftr n (f (former)) (c (cur)) (l (latter)))
    (if (zero? n)
        (begin (former f)
               (cur c)
               (latter l))
        (if (null? f)
            (shiftr (sub1 n) null 0 (unsafe-cons-list c l))
            (shiftr (sub1 n) (unsafe-cdr f) (unsafe-car f) (unsafe-cons-list c l)))))
  (define (shiftl n (f (former)) (c (cur)) (l (latter)))
    (if (zero? n)
        (begin (former f)
               (cur c)
               (latter l))
        (if (null? l)
            (shiftl (sub1 n) (unsafe-cons-list c f) 0 null)
            (shiftl (sub1 n) (unsafe-cons-list c f) (unsafe-car l) (unsafe-cdr l)))))
  (define (read n)
    (define (->byte v)
      (if (eof-object? v)
          0
          v))
    (cur
     (->byte
      (for/last ((_ (in-range n)))
        (read-byte)))))
  (define (put n)
    (define bt (cur))
    (for ((_ (in-range n)))
      (write-byte bt))))
