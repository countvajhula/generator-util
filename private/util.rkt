#lang racket/base

(require racket/stream
         (except-in data/collection
                    foldl
                    foldl/steps
                    append
                    index-of)
         relation)

(provide take-while)

(define (take-while pred seq)
  (if (empty? seq)
      (stream)
      (let ([v (first seq)]
            [vs (rest seq)])
        (if (pred v)
            (stream-cons v (take-while pred vs))
            null))))
