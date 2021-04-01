#lang racket
(require file/glob)
(provide (all-defined-out))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions (maybe move to other module)
(define mode (make-parameter 'dry))
(define overwrite-mode (make-parameter 'fail))


(define (indent breadcrumb)
  (apply string-append (for/list [(i (cdr breadcrumb))] "    ")))

(define (breadcrumb->path breadcrumb #:drop [drop-prefix 0])
  (apply build-path (reverse (if (zero? drop-prefix)
                                 breadcrumb
                                 (drop breadcrumb drop-prefix)))))

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

(define (copy-from breadcrumb #:match [from-re #f] #:replace [replace-re #f] . rest)
  (define target-file-or-dir (apply build-path (reverse (cdr breadcrumb))))

  (define src-glob-or-dir (apply build-path (cons (car breadcrumb) rest)))
  (define src-glob (glob (if (directory-exists? src-glob-or-dir)
                             (build-path src-glob-or-dir "*.*")
                             src-glob-or-dir)))

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

    (define src-time (file-or-directory-modify-seconds src-file))
    (define target-time (file-or-directory-modify-seconds target-file))
    (define src-newer? (target-time . < . src-time))
    (define-values (action-str copy? overwrite?)
      (cond
        [(not (file-exists? target-file))
         (values "Copying" #t #f)]
        [(equal? 'skip (overwrite-mode))
         (values "Skipping" #f #f)]
        [(and (equal? 'newer (overwrite-mode))
              src-newer?)
         (values "Replacing newer" #t #t)]
        [(equal? 'newer (overwrite-mode))
         (values "Skipping older" #f #f)]
        [(equal? 'overwrite (overwrite-mode))
         (values "Replacing" #t #t)]))

    (define (log prefix #:func [func printf])
      (func "~a~a  ~a -> ~a\n"
            (indent breadcrumb)
            prefix
            (path->string src-file)
            (path->string target-file)))

    (match (mode)
      ['dry (log (format "*~a" action-str))]
      ['build
       (log action-str)
       (when copy?
         (copy-file src-file target-file overwrite?))]
      ['check
       (if (and (file-exists? target-file)
                (equal? src-time target-time))
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
