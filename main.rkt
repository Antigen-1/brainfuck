#lang racket/base

(module reader syntax/module-reader
  brainfuck/expander/expander
  #:read (compose1 list bf:read)
  #:read-syntax (compose1 list bf:read-syntax)
  #:whole-body-readers? #t
  (require (prefix-in bf: "reader/reader.rkt")))
