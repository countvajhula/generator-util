#lang info
(define collection "generator-util")
(define deps '("base"
               "collections-lib"
               "relation"
               "social-contract"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"
                     "cover"
                     "cover-coveralls"
                     "collections-doc"))
(define scribblings '(("scribblings/generator-util.scrbl" ())))
(define compile-omit-paths '("dev" "coverage"))
(define clean '("compiled" "doc"))
(define pkg-desc "Useful utilities for working with generators")
(define version "1.1")
(define pkg-authors '(countvajhula))
