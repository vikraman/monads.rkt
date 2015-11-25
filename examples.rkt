#lang racket

(require "macros.rkt")

(define return-maybe
  (λ (a) `(Just ,a)))

(define bind-maybe
  (λ (ma f)
    (match ma
      [`(Just ,a) (f a)]
      ['(Nothing) '(Nothing)])))

(define zero-maybe
  (λ ()
    '(Nothing)))

(do 'maybe
    (`(,a . ,b) <- `(Just ,(cons 1 2)))
    (`(,c . ,d) <- `(Just ,(cons 3 4)) when (eqv? c 3))
    (e = 5)
    (return (+ a d e)))
