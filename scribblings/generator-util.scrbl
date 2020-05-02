#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[(rename-in racket (sequence? b:sequence?) (in-producer b:in-producer))
                    (rename-in racket/generator (generator? b:generator?) (generator-state b:generator-state))
                    generator-util
                    (only-in data/collection sequence?)
                    (only-in relation ->list fold)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require relation)
                                 '(require generator-util)
                                 '(require racket/set)
                                 '(require racket/generic)
                                 '(require (prefix-in b: racket/generator))
                                 '(require (only-in racket/generator
                                                    generator
                                                    yield))
                                 '(require racket/stream))))

@title{Generator Utilities}
@author{Siddhartha Kasivajhula}

@defmodule[generator-util]

Primitives and utilities for working with @seclink["Generators" #:doc '(lib "scribblings/reference/reference.scrbl")]{generators}.

This module provides general-purpose utilities to achieve standard sequence-style transformations with generators without losing the laziness and constant-memory guarantees that generators provide.

@elemtag["coroutines"]{These utilities are not suitable for use in all cases.} In particular, they are not suitable for use with coroutines -- i.e. in cases where there is bidirectional communication with a generator. Some of the utilities below wrap underlying generators with intermediary ones, and values sent to them are not conveyed to the underlying generators.

@section{Primitives}

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

@deftogether[(
@defproc[(generator-done? [g generator?])
         boolean?]
@defproc[(generator-empty? [g generator?])
         (values boolean? generator?)]
	 )]{

 Predicates to assert whether a generator is "empty" or "done." @racket[generator-empty?] is a statement about the "contents" of the generator, whereas @racket[generator-done?] is a statement about the "state" of the generator. This distinction is made because Racket generators evaluate to two different kinds of values -- first, the values that are @racketlink[yield]{yielded} from within the generator, and second, the return value of the generator which is not explicitly yielded. For the purposes of this interface, the yielded values are treated as the contents of the generator. Thus, if a generator yields no further values but nevertheless evaluates to a nontrivial return value, it is still considered empty. Explicitly, @racket[generator-done?] is equivalent to @racket[(eq? 'done (generator-state g))]. A generator that has exhausted all of its values but has not yet evaluated its return value is @emph{empty} but not @emph{done}.

 @racket[generator-empty?] returns both a boolean value indicating whether the generator is empty or not, as well as a fresh generator intended to supplant the original generator in the calling context. This is necessary because checking for emptiness requires invoking the generator to inspect the first element, which mutates the original generator. The returned generator is equivalent to the original generator prior to the mutation, modulo the @elemref["coroutines"]{aforementioned caveat} about coroutines.

@examples[
    #:eval eval-for-docs
	(define g (make-generator))
    (generator-done? g)
    (define-values (is-empty? g) (generator-empty? g))
	is-empty?
    (generator-done? g)
	(g)
    (generator-done? g)
  ]
}

@defproc[(generator-peek [g generator?])
         (values any/c generator?)]{

 "Peek" at the first value in the generator without modifying it. Of course, inspecting the first element in a generator must necessarily modify it. To preserve the illusion that no mutation has taken place, a generator equivalent to the original one prior to mutation is returned along with the peeked-at value. This returned generator is expected to be used in place of the original one in the calling context as it will be functionally equivalent to the original one, modulo the @elemref["coroutines"]{aforementioned caveat} about coroutines. Additionally, peeking does not protect against invocation side-effects. If invoking @racket[g] results in a side-effect, that would still happen if you peek at it. But it won't happen a second time since the replacement generator only includes the return value from the original invocation, and not the side effect.

@examples[
    #:eval eval-for-docs
	(define g (make-generator 1 2 3))
    (define-values (v g) (generator-peek g))
	v
	(g)
	(g)
	(g)
  ]
}

@section{Utilities}

@defproc[(generator-map [f (-> any/c any/c)] [g generator?])
         generator?]{

Analogous to @racket[map], yields a fresh generator whose values are the elements of @racket[g] transformed under @racket[f].

@examples[
    #:eval eval-for-docs
	(define g (make-generator 1 2 3))
	(define g (generator-map add1 g))
	(g)
	(g)
	(g)
  ]
}

@defproc[(generator-filter [f (-> any/c boolean?)] [g generator?])
         generator?]{

Analogous to @racket[filter], yields a fresh generator whose values are the elements of @racket[g] for which the predicate @racket[f] is true.

@examples[
    #:eval eval-for-docs
	(define g (make-generator 1 2 3 4 5))
	(define g (generator-filter odd? g))
	(g)
	(g)
	(g)
  ]
}

@defproc[(generator-fold [f procedure?]
                         [g generator?]
                         [base any/c undefined]
                         [#:order order (one-of/c 'abb 'bab) 'abb])
         generator?]{

Analogous to @racket[fold], yields a fresh generator whose values are the steps in the aggregation of the elements of @racket[g] under the folding function @racket[f].

@examples[
    #:eval eval-for-docs
	(define g (make-generator 1 2 3 4))
	(define g (generator-fold + g))
	(g)
	(g)
	(g)
	(g)
  ]
}

@defproc[(generator-append [a generator?] [b generator?])
         generator?]{

Analogous to @racket[append], yields a fresh generator whose values are the elements of @racket[a] followed by the elements of @racket[b].

@examples[
    #:eval eval-for-docs
	(define a (make-generator 1 2))
	(define b (make-generator 3 4))
	(define g (generator-append a b))
	(g)
	(g)
	(g)
	(g)
  ]
}

@deftogether[(
@defproc[(generator-zip [g generator?]
                        ...)
         generator?]
@defproc[(generator-zip-with [f procedure?]
                             [g generator?]
                             ...)
         generator?]
	 )]{

Analogous to @racket[zip-with], @racket[generator-zip-with] yields a fresh generator whose values are the elements of the input generators combined using the function @racket[f]. @racket[f] must accept a number of arguments equal to the number of provided generators. @racket[generator-zip] simply combines the generator values using @racket[list].

@examples[
    #:eval eval-for-docs
	(define a (make-generator 1 2))
	(define b (make-generator 'a 'b))
	(define c (make-generator 'A 'B))
	(define g (generator-zip a b c))
	(g)
	(g)
	(define a (make-generator 1 2))
	(define b (make-generator 1 2))
	(define c (make-generator 1 2))
	(define g (generator-zip-with + a b c))
	(g)
	(g)
  ]
}

@defproc[(generator-join [g generator?])
         generator?]{

Yields a fresh generator whose values are the elements of @racket[g] "flattened" by one level.

@examples[
    #:eval eval-for-docs
	(define g (make-generator (list 1) (list 2) (list 3)))
	(define g (generator-join g))
	(g)
	(g)
	(g)
  ]
}

@defproc[(generator-flatten [g generator?])
         generator?]{

Yields a fresh generator whose values are the "flattened" elements of @racket[g]. This is equivalent to repeatedly applying @racket[generator-join] until the values are no longer sequences.

@examples[
    #:eval eval-for-docs
	(define g (make-generator (list (list (list 1))) (list (list (list 2))) (list (list (list 3)))))
	(define g (generator-flatten g))
	(g)
	(g)
	(g)
  ]
}

@defproc[(yield-from [g generator?])
         any]{

Yield all values from a provided generator. This should only be used inside a generator.

@examples[
    #:eval eval-for-docs
	(define g (generator () (yield-from (make-generator 1 2 3))))
    (g)
    (g)
    (g)
  ]
}

@section{Interface}

@defthing[gen:generator any/c]{

 A @tech/reference{generic interface} for generators, that wraps built-in generators but also enables providing generator semantics in custom types.

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

 To implement this interface for custom types, the following method needs to be implemented:

 @defproc[(generator-state [v generator?])
          [symbol? (one-of/c 'fresh 'suspended 'running 'done)]]{

 Describes the state of the generator. The implementation should mirror @racketlink[b:generator-state]{generator-state}.

 }

@defproc[(generator? [v any/c])
         boolean?]{

 Predicate to check if a value is a generator. Like @racketlink[b:generator?]{generator?} but also recognizes instances of @racket[gen:generator].

@examples[
    #:eval eval-for-docs
    (generator? 3)
    (generator? (generator () (void)))
    (generator? (generator-cons 1 (generator-null)))
  ]
}

}

@defproc[(in-producer [g generator?]
                      [stop any/c undefined]
                      [v any/c] ...)
         sequence?]{

 Similar to @racketlink[b:in-producer]{in-producer}, but yields a data/collection @racket[sequence?] rather than a built-in @racketlink[b:sequence?]{sequence?}.

@examples[
    #:eval eval-for-docs
	(define g (make-generator 1 2 3))
	(->list (in-producer g (void)))
  ]
}
