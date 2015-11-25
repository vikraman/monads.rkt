#lang racket

(provide (all-defined-out))

(require (for-syntax racket/syntax))

(define-syntax do
  (λ (stx)
    (syntax-case stx (quote return <- = when)
      ((_ 'm (return e))
       (with-syntax* ((monad (syntax->datum #'m))
                      (return (format-id #'m "~a-~a" 'return #'monad)))
         #`(return e)))
      ((_ 'm (pat <- e) e* e** ...)
       (with-syntax* ((monad (syntax->datum #'m))
                      (bind (format-id #'m "~a-~a" 'bind #'monad))
                      (zero (format-id #'m "~a-~a" 'zero #'monad)))
         #`(bind e (match-lambda
                     (pat (do 'm e* e** ...))
                     (else (zero))))))
      ((_ 'm (pat <- e when guard) e* e** ...)
       (with-syntax* ((monad (syntax->datum #'m))
                      (bind (format-id #'m "~a-~a" 'bind #'monad))
                      (zero (format-id #'m "~a-~a" 'zero #'monad)))
         #`(bind e (match-lambda
                     (pat #:when guard (do 'm e* e** ...))
                     (else (zero))))))
      ((_ 'm (pat = e) e* e** ...)
       (with-syntax* ((monad (syntax->datum #'m))
                      (zero (format-id #'m "~a-~a" 'zero #'monad)))
         #`(match e
             (pat (do 'm e* e** ...))
             (else (zero)))))
      ((_ 'm e e* e** ...)
       (with-syntax* ((monad (syntax->datum #'m))
                      (bind (format-id #'m "~a-~a" 'bind #'monad)))
         #`(bind e (λ (_)
                     (do 'm e* e** ...))))))))
