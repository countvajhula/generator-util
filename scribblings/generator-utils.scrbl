#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[racket
                    racket/generator]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require relation)
                                 '(require generator-utils)
                                 '(require racket/set)
                                 '(require racket/generic)
                                 '(require racket/generator)
                                 '(require racket/stream))))

@title{Generator Utilities}
@author{Siddhartha Kasivajhula}

Primitives and utilities for working with @seclink["Generators" #:doc '(lib "scribblings/reference/reference.scrbl")]{generators}.

@defmodule[generator-utils]

@deftogether[(
@defproc[(generator-null [return any/c (void)])
         generator?]
@defproc[(generator-cons [v any/c] [g generator?])
         generator?]
@defproc[(make-generator [v any/c] ...)
         generator?]
	 )]{

  Constructors for @racketlink[generator]{generators}, analogous to @racket[null], @racket[cons], and @racket[list] for lists. @racket[generator-null] serves as the null constructor as well as the identity value in composing generators, while @racket[generator-cons] constructs a new generator from an arbitrary value and an existing generator. @racket[make-generator] is a variadic constructor analogous to @racket[list]. If a @racket[return] value is provided to @racket[generator-null], it is used as the return value of the generator once it is exhausted -- that is, as the return value for any generator with this empty generator instance at its tail. Note that these constructors are @emph{not} lazy, at least for the moment.

@examples[
    #:eval eval-for-docs
    (define g (generator-cons 1 (generator-null)))
    (g)
    (void? (g))
    (define g (generator-cons 1 (generator-null 23)))
    (g)
    (g)
    (define g (make-generator 1 2 3))
    (g)
    (->list g)
  ]
}

@defthing[gen:generator any/c]{

 A @tech/reference{generic interface} for generators.

 @examples[
    #:eval eval-for-docs
    (struct api-reader (source)
      #:transparent
      #:property prop:procedure
      (λ (self)
        ((api-reader-source self)))
      #:methods gen:generator
      [(define/generic -generator-state generator-state)
       (define (generator-state st)
         (-generator-state (api-reader-source source)))])
    (define g (api-reader (make-generator 1 2 3)))
    (g)
    (->list g)
  ]

@defproc[(generator? [v any/c])
         boolean?]{

 Predicate to check if a value is a generator.

@examples[
    #:eval eval-for-docs
    (generator? 3)
    (generator? (generator-cons 1 (generator-null)))
  ]
}

 To implement this interface for custom types, the following method needs to be implemented:

 @defproc[(generator-state [v generator?])
          [symbol? (one-of/c 'fresh 'suspended 'running 'done)]]{

 Describes the state of the generator. The implementation should mirror @racket[generator-state].

 }

}
