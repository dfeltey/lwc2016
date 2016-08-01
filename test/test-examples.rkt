#lang racket

(require racket/runtime-path
         rackunit)

(define-runtime-path programs "../examples/programs")
(define-runtime-path even-odd "../examples/even-odd/even-odd.rkt")
(define-runtime-path prefix-even-odd "../examples/even-odd/even-odd-prefix.rkt")
(define-runtime-path expanded-even-odd "../examples/even-odd/expanded-even-odd.rkt")
(define even-odd-expected-res (file->string even-odd-expected))

(define-runtime-path expected "expected")
(define-runtime-path even-odd-expected "expected/even-odd.expected")
(define EXPECTED-SUFFIX ".expected")

(define (check-file path)
  (define file-name (file-name-from-path path))
  (define expected-path
    (build-path
     expected
     (string-append
      (first (string-split (path->string file-name) "."))
      EXPECTED-SUFFIX)))
  (check-pred file-exists? expected-path)
  (define actual-res (run-file path))
  (define expected-res (file->string expected-path))
  (check-equal? actual-res expected-res))


(define (run-file path)
  (with-output-to-string
      (thunk
       (dynamic-require
        `(submod (file ,(path->string path)) main)
        #f))))


(module+ test
  (check-equal? (run-file even-odd) even-odd-expected-res)
  (check-equal? (run-file prefix-even-odd) even-odd-expected-res)
  (check-equal? (run-file expanded-even-odd) even-odd-expected-res)
  (for ([file (in-directory programs)]
        #:when (file-exists? file))
    (check-file file)))
  
                             