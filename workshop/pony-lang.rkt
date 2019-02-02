#lang racket/base

(provide
  (except-out (all-from-out racket/base) #%module-begin)
  (rename-out [module-begin #%module-begin])
  ranch)

(require
  (for-syntax
     racket/base
     syntax/stx
     syntax/parse))

(define-syntax-rule (module-begin expr)
  (#%module-begin
    (provide the-ranch)
    (define the-ranch expr)))

(define-syntax (ranch stx)
  (syntax-parse stx
    #:datum-literals (ponies pony)
    [(_ (ponies (pony name:id cry:str) ...))
     #'(lambda (pony-name)
         (cond
           [(eq? pony-name 'name) cry] ...
           (else "This pony does not exist!")))]))
