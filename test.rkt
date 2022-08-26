#lang racket

(require "main.rkt")

(module+ test (require rackunit))

(module+ test
  (check-equal? (my-match 1 [_ 2]) 2)
  (check-equal? (my-match 1 [a 2]) 2)
  (check-equal? (my-match 1 [a a]) 1))

(define-pattern-syntax t (syntax-rules () [(_ p) p]))
(module+ test
  (check-equal? (my-match 1 [(t a) a]) 1))
