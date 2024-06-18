#lang racket/base

(module reader syntax/module-reader
  brainfuck/expander/expander
  #:read bf:read
  #:read-syntax bf:read-syntax
  (require (prefix-in bf: "reader/reader.rkt")))
