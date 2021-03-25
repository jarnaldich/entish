#lang racket
(require file/glob
         "macros.rkt")
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions (maybe move to other module)
(define mode (make-parameter 'dry))

(define (indent breadcrumb)
  (apply string-append (for/list [(i breadcrumb)] "    ")))

(define (run-thunk t) (t))
(define (run-thunks ts) (map run-thunk ts))

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
    ['dry (log "*Writing")]
    ['build
     (log "Writing")
     (display-to-file txt target-file #:exists 'truncate/replace)]
    ['check
     (if (equal?
          (file->string target-file) txt)
         (log "Checking")
         (log "ERROR:" #:func eprintf))]
    [m (error "Wrong mode: ~v" m)]))

(define (copy-from breadcrumb . rest)

  (define target-file (apply build-path (reverse (cdr breadcrumb))))
  (define src-file (apply build-path (cons (car breadcrumb) rest)))

  (define (log prefix #:func [func printf])
    (func "~a~a  ~a -> ~a\n"
          (indent breadcrumb)
          prefix
          (path->string src-file)
          (path->string target-file)))

  (match (mode)
    ['dry (log "*Copying")]
    ['build
     (log "Copying")
     (copy-file src-file target-file #f)]
    ['check
     (if (file-exists? src-file)
         (log "Checking")
         (log "ERROR:" #:func eprintf))]
    [m (error "Wrong mode: ~v" m)])

  )

(define (clear breadcrumb)
  (define contents (glob (apply build-path (reverse (cons "*.*" breadcrumb)))))

  (define (log prefix f #:func [func printf])
    (func "~a~a  ~a\n"
          (indent breadcrumb)
          prefix
          (path->string f)))

  (match (mode)
    ['dry (log "*Clearing") ]
    ['build
     (for ([f contents])
       (log "Clearing" f)
       (delete-directory/files f))]
    ['check #t]
    [m (error "Wrong mode: ~v" m)])

  (void))

(define (file breadcrumb . rest)
  (define fname (apply build-path (reverse breadcrumb)))

  (define (log prefix #:func [func printf])
    (func "~a~a  ~a\n"
          (indent breadcrumb)
          prefix
          (path->string fname)))


  (match (mode)
    ['dry (log "*Creating") ]
    ['build (log "Creating")]
    ['check
     (if (file-exists? fname)
         (log "Checking")
         (log "ERROR:" #:func eprintf))]
    [m (error "Wrong mode: ~v" m)])

  (run-thunks rest)

  fname)

(define (dir breadcrumb . rest)

  (define dir-name (apply build-path (reverse breadcrumb)))

  (define (log prefix #:func [func printf])
    (func "~a~a  ~a\n"
          (indent breadcrumb)
          prefix
          (path->string dir-name)))

  (match (mode)
    ['dry (log "*Creating") ]
    ['build
     (log "Creating")
     (make-directory* dir-name)]
    ['check
     (if (directory-exists? dir-name)
         (log "Checking")
         (log "ERROR:" #:func eprintf))]
    [m (error "Wrong mode: ~v" m)])

  (run-thunks rest)
  ; return the artifact
  dir-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions
