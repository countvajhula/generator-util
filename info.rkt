#lang info
(define collection "generator-utils")
(define deps '("base"
               "collections-lib"
               "functional-utils"
               "relation"))
(define build-deps '("scribble-lib"
                     "scribble-abbrevs"
                     "racket-doc"
                     "rackunit-lib"
                     "sandbox-lib"))
(define scribblings '(("scribblings/generator-utils.scrbl" (multi-page))))
(define clean '("compiled" "doc"))
(define pkg-desc "Useful utilities for working with generators")
(define version "0.0")
(define pkg-authors '(countvajhula))
