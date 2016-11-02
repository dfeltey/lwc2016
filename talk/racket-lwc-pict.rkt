#lang racket

(provide giant-duck-pict)

(require racket/runtime-path
         pict
         slideshow)

(define-runtime-path giant-duck "clipart/giant-duck.jpg")

(define giant-duck-pict
  (let* ([pict (bitmap giant-duck)]
         [scale-x (/ 1024 (pict-width pict))]
         [scale-y (/ 768 (pict-height pict))])
    (scale pict scale-x scale-y)))