#lang scribble/doc
@require[scribble/manual
         scribble-abbrevs/manual
         scribble/example
         racket/sandbox
         @for-label[(rename-in racket (sequence? b:sequence?) (in-producer b:in-producer))
                    (only-in racket/generator (generator b:generator) (generator? b:generator?) (generator-state b:generator-state))
                    generator-util
                    (only-in data/collection sequence? cycle repeat gen:collection)
                    (only-in relation ->list ->generator fold :)]]

@(define eval-for-docs
  (parameterize ([sandbox-output 'string]
                 [sandbox-error-output 'string]
                 [sandbox-memory-limit #f])
                 (make-evaluator 'racket/base
                                 '(require relation
                                           generator-util
                                           racket/set
                                           racket/generic
                                           (prefix-in b: racket/generator)
                                           (only-in racket range)
                                           racket/stream))))

@title{Generator Utilities}
@author{Siddhartha Kasivajhula}

@defmodule[generator-util]

Primitives and utilities for working with @seclink["Generators" #:doc '(lib "scribblings/reference/reference.scrbl")]{generators}.

This module provides general-purpose utilities to achieve standard "list-like" transformations with generators without losing the laziness and constant-memory guarantees that generators provide. Alternative bindings for many of those found in @seclink["Generators" #:doc '(lib "scribblings/reference/reference.scrbl")]{@racket[racket/generator]} are included here, so that importing both is generally not necessary.

@elemtag["coroutines"]{@bold{Caveat}}: These utilities are not suitable for use with coroutines, i.e. in cases where there is bidirectional communication with a generator. This is because the utilities wrap underlying generators with intermediary ones in some cases, and values sent to them are not conveyed to the underlying generators.

@section{Primitives}

@deftogether[(
@defproc[(generator-null [return any/c (void)])
         generator?]
@defproc[(generator-cons [v any/c] [g generator?])
         generator?]
@defproc[(make-generator [v any/c] ...)
         generator?]
	 )]{

  Constructors for @racketlink[b:generator]{generators}, analogous to @racket[null], @racket[cons], and @racket[list] for lists. @racket[generator-null] serves as the null constructor as well as the identity value in composing generators, while @racket[generator-cons] constructs a new generator from an arbitrary value and an existing generator. @racket[make-generator] is a variadic constructor analogous to @racket[list]. If a @racket[return] value is provided to @racket[generator-null], it is used as the return value of the generator once it is exhausted -- that is, as the return value for any generator with this empty generator instance at its tail.

  Note that these constructors are @emph{not} lazy. In order to construct a generator from a sequence lazily, use @racket[generate] instead.

  Also see @racket[gen] for convenient ways to use these constructors.

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
    (->list (in-producer g (void)))
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

 In general, favor using @racket[generator-done?].

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

Analogous to @racket[map], returns a fresh generator whose values are the elements of @racket[g] transformed under @racket[f].

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

Analogous to @racket[filter], returns a fresh generator whose values are the elements of @racket[g] for which the predicate @racket[f] is true.

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

Analogous to @racket[fold], returns a fresh generator whose values are the steps in the aggregation of the elements of @racket[g] under the folding function @racket[f].

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

Analogous to @racket[append], returns a fresh generator whose values are the elements of @racket[a] followed by the elements of @racket[b].

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

@defproc[(generator-cycle [g generator?] [stop any/c (void)])
         generator?]{

Analogous to @racket[cycle], returns an infinite generator whose values are the elements of @racket[g], repeated when exhausted. If a @racket[stop] value is provided, the elements are drawn from @racket[g] until @racket[stop] is encountered. This utility uses memory proportional to the size of the repeated sequence.

@examples[
    #:eval eval-for-docs
	(define g (generator-cycle (make-generator 1 2 3)))
	(g)
	(g)
	(g)
	(g)
	(g)
  ]
}

@defproc[(generator-repeat [v any/c])
         generator?]{

Analogous to @racket[repeat], returns an infinite generator whose values are @racket[v] repeated indefinitely.

@examples[
    #:eval eval-for-docs
	(define g (generator-repeat 5))
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

Analogous to @racket[zip-with], @racket[generator-zip-with] returns a fresh generator whose values are the elements of the input generators combined using the function @racket[f]. @racket[f] must accept a number of arguments equal to the number of provided generators. @racket[generator-zip] simply combines the generator values using @racket[list]. The generation stops when one of the input generators runs out of values.

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

@defproc[(generator-interleave [g generator?]
							   ...)
         generator?]{

Returns a fresh generator whose values are the elements of the input generators taken in turn, one at a time. The generation stops when one of the input generators runs out of values.

@examples[
    #:eval eval-for-docs
	(define g (generator-interleave (make-generator 1 2 3) (make-generator 4 5 6)))
	(g)
	(g)
	(g)
	(g)
	(g)
	(g)
  ]
}

@defproc[(generator-join [g generator?])
         generator?]{

Returns a fresh generator whose values are the elements of @racket[g] "flattened" by one level.

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

Returns a fresh generator whose values are the "flattened" elements of @racket[g]. This is equivalent to repeatedly applying @racket[generator-join] until the values are no longer sequences.

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

@defproc[(generate [seq sequence?] [return any/c (void)])
         generator?]{

Returns a generator that generates @racket[seq]. See @racket[->generator].

@examples[
    #:eval eval-for-docs
    (define g (generate (range 5 10)))
	(g)
	(g)
	(g)
    (define g (generate "apple"))
	(g)
	(g)
	(g)
  ]
}

@section{Interface}

This module provides a generic interface @racket[gen:generator] representing a generator, as well as a particular generator type @racket[gen] implementing that interface. The former allows supporting generator semantics in custom types and the latter is a rich generator type intended as a drop-in replacement for built-in generators, and which comes with some conveniences.

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
    (->list (in-producer g (void)))
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

@defstruct[gen ([primitive generator?])
               #:omit-constructor]{
  A type representing a generator. All generators returned by this module are of this type, and they should function identically to built-in generators, except that this type also implements @racket[gen:collection], enabling it to be treated as a generic collection of values, and supporting convenient construction using the generic construction operator @racket[:].
@itemlist[
@item{@racket[primitive] - An underlying generator object, which would typically be a built-in @racketlink[b:generator]{@racket[generator]}, but could also be a @racket[gen].}]

@examples[
    #:eval eval-for-docs
	(define g1 (: 1 2 (generator-null)))
	(define g2 (: 3 4 5 g1))
	(g2)
	(g2)
	(g2)
	(g2)
	(g2)
  ]
}

@defform[(generator formals body ...)]{
  Identical to @racketlink[b:generator]{@racket[generator]} except that it produces a @racket[gen] rather than a built-in generator type.
}


@defproc[(in-producer [g generator?]
                      [stop any/c undefined]
                      [v any/c] ...)
         sequence?]{

 Similar to @racketlink[b:in-producer]{in-producer}, but returns a data/collection @racket[sequence?] rather than a built-in @racketlink[b:sequence?]{sequence?}.

@examples[
    #:eval eval-for-docs
	(define g (make-generator 1 2 3))
	(->list (in-producer g (void)))
  ]
}
