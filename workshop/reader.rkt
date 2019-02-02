#lang racket/base

(provide (rename-out [pony-read read]
                     [pony-read-syntax read-syntax]))

(require
  racket/port
  syntax/strip-context
  brag/support
  "parser.rkt")

(define (pony-read in)
  (syntax->datum
    (pony-read-syntax #f in)))

(define (pony-read-syntax src in)
  (with-syntax ([parse-tree (parse src (make-tokenizer in src))])
    (strip-context
      #'(module pony-file workshop/expander
          parse-tree))))

(define (make-tokenizer port [path #f])
  (port-count-lines! port)
  (lexer-file-path path)
  (define (next-token)
    (define asm-lexer
      (lexer-srcloc
        [(eof) (return-without-srcloc eof)]
        ["\n" (token 'NEWLINE)]
        ["R[" (token 'RANCH-BEGIN)]
        ["]R" (token 'RANCH-END)]
        ["P[" (token 'PONY-BEGIN)]
        [(:+ (:& (:~ "\n") whitespace)) (token 'SPACE)]
        [(:>= 2 alphabetic) (token 'ID (string->symbol lexeme))]
        [(from/to "\"" "\"") (token 'STRING (trim-ends "\"" lexeme "\""))]
        [any-char (next-token)]))
    (asm-lexer port))
  next-token)
