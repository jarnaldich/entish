#lang racket
(require file/glob)
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions (maybe move to other module)
(define mode (make-parameter 'dry))

(define (indent breadcrumb)
  (apply string-append (for/list [(i (cdr breadcrumb))] "    ")))

(define (breadcrumb->path breadcrumb #:drop [drop-prefix 0])
  (apply build-path (reverse (if (zero? drop-prefix)
                                 breadcrumb
                                 (drop breadcrumb drop-prefix)))))

(define (run-thunk t) (t))
(define (run-thunks ts) (map run-thunk ts))

(struct entish-node (breadcrumb children)
  #:transparent)

(struct copy* entish-node ()
  #:guard (lambda (breadcrumb children type-name)
            (displayln "Validating copy-node")
            (values breadcrumb children))
  #:property prop:procedure
  (lambda (self args)
    (displayln "Validating"))
  #:transparent)

;;; Result types
(struct result ())
(struct result-ok result (artifacts))
(struct result-error result (message))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node functions

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

(define (copy-from breadcrumb #:match [from-re #f] #:replace [replace-re #f] . rest)
  (define target-file-or-dir (apply build-path (reverse (cdr breadcrumb))))
  (define src-glob (glob (apply build-path (cons (car breadcrumb) rest))))

  (define (src->target src-file)
    ;; Directories are created before its children, so if it's
    ;; a directory path it should already exist
    (cond [(directory-exists? target-file-or-dir)
           (build-path target-file-or-dir (file-name-from-path src-file))]
          [else target-file-or-dir]))

  (for ([src-file src-glob]
        #:when (if from-re
                   (regexp-match from-re src-file)
                   #t))
    (define target-file (src->target (if (and from-re replace-re)
                                         (regexp-replace from-re (path->string src-file) replace-re)
                                         src-file)))
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
       (if (file-exists? target-file)
           (log "Checking")
           (log "ERROR:" #:func eprintf))]
      [m (error "Wrong mode: ~v" m)]))

  )

(define (delete breadcrumb #:match [from-re #f] #:replace [replace-re #f])
  (define dir-or-glob (breadcrumb->path breadcrumb))
  (define src-glob (cond
                     [(directory-exists? dir-or-glob)
                      (glob (breadcrumb->path (cons "*.*" breadcrumb)))]
                     [else (glob dir-or-glob)]))

  (define (log prefix f #:func [func printf])
    (func "~a~a  ~a\n"
          (indent breadcrumb)
          prefix
          (path->string f)))

  (for ([src-file src-glob]
        #:when (if from-re
                   (regexp-match from-re src-file)
                   #t))

    (define target-file  (if (and from-re replace-re)
                             (regexp-replace from-re (path->string src-file) replace-re)
                             src-file))

    (match (mode)
      ['dry  (log "*Clearing" target-file) ]
      ['build
       (log "Clearing" target-file)
       (delete-directory/files target-file)]
      ['check #t]
      [m (error "Wrong mode: ~v" m)]))

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

(define (root breadcrumb . rest)
  (define root (car breadcrumb))
  (printf "Root: ~a\n" root)
  (run-thunks rest))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions
