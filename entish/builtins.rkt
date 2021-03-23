#lang racket
(require "macros.rkt")
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions (maybe move to other module)

(define mode (make-parameter 'check))

(define (indent breadcrumb)
  (apply string-append (for/list [(i breadcrumb)] "    ")))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions

(define (template-string breadcrumb . rest)
  
  (define target-file (apply build-path (reverse (cdr breadcrumb))))
  (define txt (car breadcrumb))

  (define (log prefix #:func [func printf])
    (func "~a~a template for ~a (~a...)\n"
            (indent breadcrumb)
            prefix
            (path->string target-file)
            (substring (string-trim txt) 0 (min 10 (string-length txt)))))
  
  (match (mode)
    ['build
     (log "Writing")
     (display-to-file txt target-file #:exists 'truncate/replace)]
    ['check
     (if (equal?
          (file->string target-file) txt)
         (log "Checking")
         (log "ERROR:" #:func eprintf))]
    [else (log "Writing")])
  )

(define (copy-from file-name . rest)
  (printf "       Copy-From ~v ~v\n" file-name rest))

(define (zip file-name . rest)
  (printf "   Zip ~v\n" file-name))

(define (file file-name . rest)
  (map (lambda (x) (x)) rest)
  (printf "   File ~v\n" file-name))

(define (dir dir-name . rest)
  (printf "Dir: ~v\n" dir-name)
  (map (lambda (x) (x)) rest)
  (printf "Dir: ~v ~v\n" dir-name (map (lambda (x) (x 'name)) rest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions
(module+ test
  (require rackunit)
  (require "macros.rkt")
  
  (test-begin
   (forest (roots (tmp (find-system-path 'temp-dir)))
            
           (tmp (file "prova.txt"
                      (template-string "PrOva"))
                (dir "subdir"
                     (file "copy_of_prova.txt"
                           (copy-from tmp "prova.txt"))))

           )

   (check-equal?
    (file->string (build-path (find-system-path 'temp-dir) "prova.txt"))
    "PrOva")
    
   )
  )

(find-system-path 'temp-dir)

