#lang racket/base
(require (submod racket/performance-hint begin-encourage-inline)
         racket/unsafe/ops
         syntax/parse/define
         (for-syntax racket/base racket/fixnum))
(provide add sub shiftr shiftl read put cur (for-syntax dispatch-+ dispatch-- dispatch-and))

;; former current latter
;; (vector/c list? byte? list?)
(define main-vector (vector null 0 null))
(define-syntax-rule (make-observer ind)
  (case-lambda
    (() (unsafe-vector*-ref main-vector ind))
    ((v) (unsafe-vector*-set! main-vector ind v))))

(begin-encourage-inline
  (define cur (make-observer 1))
  (define former (make-observer 0))
  (define latter (make-observer 2))

  (define (n:+ v1 v2)
    (bitwise-and (+ v1 v2) 255))
  (define (f:+ v1 v2)
    (unsafe-fxand (unsafe-fx+/wraparound v1 v2) 255))
  (define (n:- v1 v2)
    (bitwise-and (- v1 v2) 255))
  (define (f:- v1 v2)
    (unsafe-fxand (unsafe-fx-/wraparound v1 v2) 255))
  (define-for-syntax (dispatch-+ . ns)
    (if (andmap (compose1 fixnum-for-every-system? syntax->datum) ns)
        #'f:+
        #'n:+))
  (define-for-syntax (dispatch-- . ns)
    (if (andmap (compose1 fixnum-for-every-system? syntax->datum) ns)
        #'f:-
        #'n:-))
  (define-for-syntax (dispatch-and . ns)
    (if (andmap (compose1 fixnum-for-every-system? syntax->datum) ns)
        #'unsafe-fxand
        #'bitwise-and))

  ;; n: exact-positive-integer?
  (define-syntax-parser add
    ((_ n)
     #`(cur (#,(dispatch-+ #'n) (cur) n))))
  (define-syntax-parser sub
    ((_ n)
     #`(cur (#,(dispatch-- #'n) (cur) n))))
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
    (if (= n 1)
        (cur (->byte (read-byte)))
        (begin (read-byte) (read (sub1 n)))))
  (define (put n (bt (cur)))
    (if (= 1 n)
        (write-byte bt)
        (begin (write-byte bt) (put (sub1 n) bt)))))
