#lang racket/base

(require racket/undefined
         racket/function
         racket/stream
         racket/match
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         relation)

(provide unthunk
         undefined?
         !!
         flip
         take-while
         any?)

(define (orf . args)
  (match args
    ['() #f]
    [(cons v vs)
     (match vs
       ['() v]
       [_ (or v (apply orf vs))])]))

(define any? (curry foldl
                    orf
                    #:into #f
                    #:order 'bab))

(define (unthunk f . args)
  (λ ignored-args
    (apply f args)))

(define (undefined? v)
  (eq? v undefined))

(define !! negate)

(define (flip f)
  (λ (x y . args)
    (apply f y x args)))

(define (take-while pred seq)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (stream-cons v (take-while pred vs))
            null))))
