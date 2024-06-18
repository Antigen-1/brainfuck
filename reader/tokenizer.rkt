#lang racket/base
(require brag/support)
(provide make-token-producer)

(define (make-token type val srcloc #:skip? skip?)
  (token type val
         #:line (srcloc-line srcloc)
         #:column (srcloc-column srcloc)
         #:position (srcloc-position srcloc)
         #:span (srcloc-span srcloc)
         #:skip? skip?))

(define-lex-abbrev keyword (union #\> #\< #\. #\, #\+ #\- #\[ #\]))

(define tokenize
  (lexer
   [(union #\> #\<) (make-token 'PTROP (string->symbol lexeme) lexeme-srcloc #:skip? #f)]
   [(union #\+ #\- #\. #\,) (make-token 'SLOTOP (string->symbol lexeme) lexeme-srcloc #:skip? #f)]
   [#\[ (make-token 'LOOPSTART #f lexeme-srcloc #:skip? #f)]
   [#\] (make-token 'LOOPEND #f lexeme-srcloc #:skip? #f)]
   [(char-complement keyword) (make-token 'OTHER #f lexeme-srcloc #:skip? #t)]
   [(eof) (void)]))

(define (make-token-producer port)
  (lambda () (tokenize port)))

(module+ test
  (require rackunit)
  (define s (open-input-string ">.,+-[]"))
  (define p (make-token-producer s))
  (check-equal? (token-struct-type (p)) 'PTROP)
  (check-equal? (token-struct-type (p)) 'SLOTOP)
  (check-equal? (token-struct-type (p)) 'SLOTOP)
  (check-equal? (token-struct-val (p)) '+)
  (check-equal? (token-struct-val (p)) '-)
  (check-equal? (token-struct-type (p)) 'LOOPSTART)
  (check-equal? (token-struct-type (p)) 'LOOPEND))
