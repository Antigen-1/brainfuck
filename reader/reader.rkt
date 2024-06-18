#lang racket/base
(require "parser.rkt" "tokenizer.rkt")
(provide read-syntax read)
(define (read-syntax (name #f) (in (current-input-port)))
  (define real-name (if name name (object-name in)))
  (parse real-name (make-token-producer in)))
(define (read (in (current-input-port)))
  (syntax->datum (read-syntax #f in)))

(module+ test
  (require rackunit)
  (define p (open-input-string ", [->+<]."))
  (check-equal? (read p)
                '(program
                  (unit (slotop |,|))
                  (unit
                    (loopstart #f)
                    (unit (slotop -))
                    (unit (ptrop >))
                    (unit (slotop +))
                    (unit (ptrop <))
                    (loopend #f))
                  (unit (slotop |.|)))))
