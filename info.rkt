#lang info
(define collection 'multi)
(define deps '("base"
               "collections-lib"
               "relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "collections-doc"))
(define compile-omit-paths '("dev" "tests"))
(define test-include-paths '("tests"))
(define clean '("compiled" "doc"))
(define pkg-desc "Useful utilities for working with generators")
(define version "0.0")
(define pkg-authors '(countvajhula))
