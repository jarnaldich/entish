#lang racket
(require (for-syntax "macros.rkt"))
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions

(define (template-string file-name . rest)
  (printf "       Template-string ~v ~v\n" file-name rest))

(define (copy-from file-name . rest)
  (printf "       Copy-From ~v ~v\n" file-name rest))

(define (zip file-name . rest)
  (printf "   Zip ~v\n" file-name))

(define (file file-name . rest)
  (map (lambda (x) (x)) rest)
  (printf "   File ~v\n" file-name))

(define (dir dir-name . rest)
  ;(printf "Dir: ~v\n" dir-name)
  (map (lambda (x) (x)) rest)
  (printf "Dir: ~v ~v\n" dir-name (map (lambda (x) (x 'name)) rest)))

