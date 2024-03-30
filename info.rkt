#lang info
(define collection "generator-util")
(define deps '("base"
               "collections-lib"
               "relation-lib"
               "social-contract"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "relation-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "cover"
                     "cover-coveralls"
                     "collections-doc"))
(define scribblings '(("scribblings/generator-util.scrbl" ())))
(define compile-omit-paths '("dev" "tests" "coverage"))
(define test-include-paths '("tests"))
(define clean '("compiled" "doc" "tests/compiled"))
(define pkg-desc "Useful utilities for working with generators")
(define version "1.1")
(define pkg-authors '(countvajhula))
