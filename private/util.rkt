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
         !!
         flip
         take-while)

(define (unthunk f . args)
  (λ ignored-args
    (apply f args)))

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
