#lang info
(define collection "generator-util")
(define deps '("base"
               "collections-lib"
               "relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "collections-doc"))
(define scribblings '(("scribblings/generator-util.scrbl" ())))
(define compile-omit-paths '("dev"))
(define clean '("compiled" "doc"))
(define pkg-desc "Useful utilities for working with generators")
(define version "1.0")
(define pkg-authors '(countvajhula))
