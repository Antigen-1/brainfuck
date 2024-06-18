#lang racket/base
(provide add sub shiftr shiftl read put cur)

;; former current latter
;; (vector/c list? byte? list?)
(define main-vector (vector null 0 null))

;; n: exact-positive-integer?
(define (add n)
  (vector-set!
   main-vector 1
   (bitwise-and (+ n (vector-ref main-vector 1)) 255)))
(define (sub n)
  (vector-set!
   main-vector 1
   (bitwise-and (- (vector-ref main-vector 1) n) 255)))
(define (shiftr n)
  (let loop ((f (vector-ref main-vector 0))
             (c (vector-ref main-vector 1))
             (l (vector-ref main-vector 2))
             (i n))
    (if (zero? n)
        (begin (vector-set! main-vector 0 f)
               (vector-set! main-vector 1 c)
               (vector-set! main-vector 2 l))
        (if (null? f)
            (loop null 0 (cons c l) (sub1 i))
            (loop (cdr f) (car f) (cons c l) (sub1 i))))))
(define (shiftl n)
  (let loop ((f (vector-ref main-vector 0))
             (c (vector-ref main-vector 1))
             (l (vector-ref main-vector 2))
             (i n))
    (if (zero? n)
        (begin (vector-set! main-vector 0 f)
               (vector-set! main-vector 1 c)
               (vector-set! main-vector 2 l))
        (if (null? l)
            (loop (cons c f) 0 null (sub1 i))
            (loop (cons c f) (car l) (cdr l) (sub1 i))))))
(define (read n)
  (for/last ((_ (in-range n)))
    (vector-set! main-vector 1 (read-byte))))
(define (put n)
  (for ((_ (in-range n)))
    (write-byte (vector-ref main-vector 1))))
(define (cur)
  (vector-ref main-vector 1))
