#lang racket
(require file/glob
         file/md5
         uuid
         graph)

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
(define (run-thunks ts)
  (result-ok
   (for/fold ([acum '()])
             ([t ts])
     (match (run-thunk t)
       [(result-ok deps) (append deps acum)]
       [else (error "Result is not ok")]))))

(define (can-copy-newer? src-file target-file)

  (if (file-exists? target-file)
      (< (file-or-directory-modify-seconds target-file)
         (file-or-directory-modify-seconds src-file))
      #t))

(define (timestamps-match? src-file target-file)
  (if (file-exists? target-file)
      (equal? (file-or-directory-modify-seconds target-file)
              (file-or-directory-modify-seconds src-file))
      #f))

;;XXX: Maybe use raise-user-error instead???
(define (panic! tpl . args)
  (apply eprintf (list tpl args))
  (exit -1))


;;; Result types
;; May want to enforce that a node returns a result, hence the inheritance.
(struct result ())
(struct result-ok result (artifacts) #:transparent)
(struct result-error result (message))

;;; Dependency graph structs
; uid is a unique identifier for the artifact, name is the screen name for printing, etc...
; Use methods: gen:equal+hash and gen:custom-write
(struct artifact (uid name) #:transparent

  #:methods gen:equal+hash
  [;; Equal if uids are equal
   (define (equal-proc a b equal?-recur)
     (equal?-recur (artifact-uid a) (artifact-uid b)))

   (define (hash-proc a hash-recur)
     (hash-recur (artifact-uid a)))

   (define (hash2-proc a hash2-recur)
     (hash2-recur (artifact-uid a)))]

  #:methods gen:custom-write
  [(define (write-proc a port mode)
     (fprintf port "~a" (artifact-name a)))])

(define (path-artifact path)
  (artifact path
            (path->string path)))

(define (unique-artifact name)
  (artifact (uuid-string)
            name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Node functions

(define (template-string breadcrumb . rest)

  (define target-file (apply build-path (reverse (cdr breadcrumb))))
  (define txt (apply string-append (cons (car breadcrumb) rest)))

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
    [m (error "Wrong mode: ~v" m)])

  (define uid (md5 txt))
  (result-ok (list (list (artifact uid "template-string")
                         (path-artifact target-file)))))

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

  (define deps 
    (for/list ([src-file src-glob]
               #:when (if from-re
                          (regexp-match from-re src-file)
                          #t))
      (define target-file (src->target
                           (if (and from-re replace-re)
                               (regexp-replace from-re (path->string src-file) replace-re)
                               src-file)))

      (define-values (action-str copy? overwrite?)
        (cond
          [(not (file-exists? target-file))
           (values "Copying" #t #f)]
          [(equal? 'skip (overwrite-mode))
           (values "Skipping" #f #f)]
          [(and (equal? 'newer (overwrite-mode))
                (can-copy-newer? src-file target-file))
           (values "Replacing newer" #t #t)]
          [(equal? 'newer (overwrite-mode))
           (values "Skipping older" #f #f)]
          [(equal? 'overwrite (overwrite-mode))
           (values "Replacing" #t #t)]
          [(equal? 'fail (overwrite-mode))
           (panic! "Not overwiting file ~a~n You may look into -s/-o/-n overwrite cmdline switches" target-file)
           ]))

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
         (if (timestamps-match? src-file target-file)
             (log "Checking")
             (log "ERROR:" #:func eprintf))]
        [m (error "Wrong mode: ~v" m)])

      (list (path-artifact src-file)
            (path-artifact target-file))))

  (result-ok deps))

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

  (define deps
    (for/list ([src-file src-glob]
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
        [m (error "Wrong mode: ~v" m)])

      (list (path-artifact src-file) 
            (unique-artifact "*thrash*"))))

  (result-ok deps))

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

  (run-thunks rest))

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

  (run-thunks rest))

(define (root breadcrumb . rest)
  (define root (car breadcrumb))
  (printf "Root: ~a\n" root)
  (define res (run-thunks rest))
  (match res
    [(result-ok deps)
     (unweighted-graph/directed deps)]))

(define (+seq+ breadcrumb . thunks)

  (define (add-src-seq-node seq-artifact dep)
    (list seq-artifact (first dep)))

  (define (add-target-seq-node seq-artifact dep)
    (list (second dep) seq-artifact))

  (define-values (deps _ __)
    (for/fold ([acum '()]
               [old-seq-artifact null]
               [seq-artifact (unique-artifact "*seq*")])
              ([t thunks])
      (match (run-thunk t)
        [(result-ok deps) (values
                           (append acum
                                   (if (null? old-seq-artifact)
                                       '()
                                       (map ((curry add-src-seq-node) old-seq-artifact) deps)
                                       )
                                   deps
                                   (map ((curry add-target-seq-node) seq-artifact) deps))
                           seq-artifact
                           (unique-artifact "*seq*"))]
        [else (error "Result is not ok")])))

  (result-ok deps))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions
