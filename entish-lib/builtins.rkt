#lang racket
(require file/glob
         file/md5
         uuid
         racket/generator
         graph)

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core functions (maybe move to other module)
(define mode (make-parameter 'dry))
(define overwrite-mode (make-parameter 'fail))
(define trail (make-parameter '()))

(define $@ '$@)

(define ($< breadcrumb . rest)
  (lambda (breadcrumb-suffix)
    (apply build-path (append (drop (reverse breadcrumb) breadcrumb-suffix)
                              rest))))

(define (indent breadcrumb)
  (apply string-append (for/list [(i (cdr breadcrumb))] "    ")))

(define (target)
  (apply build-path (reverse (trail))))

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

(define (target-is-older? target-file src-files)
  (findf (lambda (src-file)
           (can-copy-newer? src-file target-file))
         src-files))

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

(define-syntax (defnode stx)
  (syntax-case stx (content)
    ;; Branch (regular nodes) are passed a first *breadcrumb* parameter that can be
    ;; having to mungle with it every time is somewhat cumbersome, so here we hide it into
    ;; a var named **breadcrumb**. Same for children *thunks*
    [(_ (node-name params ...) body ...)
     (with-syntax ([trail (datum->syntax stx 'trail)]
                   [children (datum->syntax stx '**children-thunks**)])
       #`(define (node-name params ... . children)
           (define deps
             (if (generator? (car (trail)))
                 (for/fold ([acum '()])
                           ([genval (in-producer (car (trail)) (void))])
                   (parameterize ([trail (cons genval (cdr (trail)))])
                     (let* ([res (begin
                                   body ...)])
                       (match res
                         [(result-ok deps) (append deps acum)]
                         [else (error "Result is tot ok")]))))
                 ;; else
                 (let* ([res (begin body ...)])
                   (match res
                     [(result-ok deps) deps]
                     [else (error "Result is tot ok")]))))

           (result-ok deps)))]

    ;; Content nodes take file name from parent, they are given a breadcrumb
    ;; that is actually the first param... Some mungling is needed to make their definition
    ;; more convenient.
    [(_ content (node-name params ...) body ...)
     (with-syntax ([children (datum->syntax stx '**children-thunks**)]
                   ;; Account for the case where there is no first param...
                   [trail (datum->syntax stx 'trail)]
                   [first-param (datum->syntax stx (let ([l (syntax->list #`(params ...))])
                                                     (if (pair? l)
                                                         (car l)
                                                         '**dummy**)))])
       #`(define (node-name params ... . children)
           (parameterize ([trail (cdr (trail))])
             (let* ([l (list params ...)]
                    [deps
                     (if (generator? (car (trail)))
                         (for/fold ([acum '()])
                                   ([genval (in-producer (car (trail)) (void))])
                           (parameterize ([trail (cons genval (trail))])
                             (let* ([res (begin body ...)])
                               (match res
                                 [(result-ok deps) (append deps acum)]
                                 [else (error "Result is tot ok")]))))
                         ;; else
                         (let* ([res (begin body ...)])
                           (match res
                             [(result-ok deps) deps]
                             [else (error "Result is tot ok")])))])

               (result-ok deps)))))]))

(define dummy-gen
  (sequence->generator (list "1st" "2nd" "3rd")))

(defnode (dummy param1 param2)

  (printf "dummy{trail=~a,params=~a,target=~a}\n" (trail)
          (list param1 param2)
          (target))

  (result-ok '()))

(defnode content (dummy-content param1 param2)

  (printf "content{trail=~a,params=~a,target=~a}\n" (trail)
          (list param1 param2)
          (target))

  (result-ok '()))

(define (make-logger node-desc)
  (lambda (action #:func [func printf])
    (func "~a~a ~a ~a\n"
          (indent (trail))
          action
          node-desc
          (path->string (target)))))

(defnode content (template)
  (let ([txt (apply string-append **children-thunks**)]
        [log (make-logger "template for" )])

    (match (mode)
      ['dry (log "*Writing")]
      ['build
       (log "Writing")
       (display-to-file txt (target) #:exists 'truncate/replace)]
      ['check
       (if (equal?
            (file->string (target)) txt)
           (log "Checking")
           (log "ERROR:" #:func eprintf))]
      [m (error "Wrong mode: ~v" m)])

    (define uid (md5 txt))
    (result-ok (list (list (artifact uid "template-string")
                           (path-artifact (target)))))))

(define (template-string . rest)
  (define breadcrumb (trail))

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

(define (copy-from #:match [from-re #f] #:replace [replace-re #f] . rest)

  (define breadcrumb (trail))
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

(define (delete #:match [from-re #f] #:replace [replace-re #f])
;; XXX: This dir-or-glob should probably be rewritten once generators work

  (define breadcrumb (trail))
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

(defnode (file*)
  (let ([log (make-logger "file named")])

    (match (mode)
      ['dry (log "*Creating") ]
      ['build (log "Creating")]
      ['check
       (if (file-exists? (target))
           (log "Checking")
           (log "ERROR:" #:func eprintf))]
      [m (error "Wrong mode: ~v" m)])

    (run-thunks **children-thunks**)))

(define (file . rest)

  (define breadcrumb (trail))
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

(define (dir . rest)

  (define breadcrumb (trail))
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

(define (root . rest)

  (define breadcrumb (trail))

  (define root (car breadcrumb))
  (define dir-name (build-path root))
  (printf "Root: ~a\n" root)

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


  (define res (run-thunks rest))
  (match res
    [(result-ok deps)
     (unweighted-graph/directed deps)]))

(define (+seq+ . thunks)

  (define breadcrumb (trail))
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

(define (+esc+ . rest)
  ((car (trail))))

(define (exec #:target-marker [target-marker '$@] . rest)

  (define breadcrumb (trail))
  (define target-file (apply build-path (reverse (cdr breadcrumb))))
  (define program (car breadcrumb))

  ;; XXX es pot fer que la funcio $f retorni un struct literal que marqui que es un input,
  ;; de manera que es pugui usar en el sistema de deps...
  (define-values (deps src-files reversed-args)
    (for/fold ([deps '()]
               [src-files '()]
               [args '()])
              ([p rest])
      (cond
        [(and (procedure? p)
              (eq? (p 'name)
                   '$<))
         ; Add it to both args an deps
         (let ([input-path ((p) (length breadcrumb))])
           (values (cons (list (path-artifact input-path)
                               (path-artifact target-file))
                         deps)
                   (cons input-path src-files)
                   (cons (path->string input-path) args)))]
        [(eq? p target-marker)
         (values deps
                 src-files
                 (cons
                  (path->string target-file)
                   args))]
        [else (values deps src-files (cons p args))])))

  (define program-args (reverse reversed-args))
  (define program-args-str (string-join program-args " "))
  (define command (string-append program " " program-args-str))

  (define (log prefix #:func [func printf])
        (func "~a~a  ~a -> ~a\n"
              (indent breadcrumb)
              prefix
              command
              (path->string target-file)))

  (define-values (action-str exec? overwrite?)
    (cond
      [(not (file-exists? target-file))
       (values "EXEC:" #t #f)]
      [(equal? 'skip (overwrite-mode))
       (values "Skipping" #f #f)]
      [(and (equal? 'newer (overwrite-mode))
            (target-is-older? target-file src-files))
       (values "Replacing newer" #t #t)]
      [(equal? 'newer (overwrite-mode))
       (values "Skipping older" #f #f)]
      [(equal? 'overwrite (overwrite-mode))
       (values "Replacing" #t #t)]
      [(equal? 'fail (overwrite-mode))
       (panic! "Not overwiting file ~a~n You may look into -s/-o/-n overwrite cmdline switches" target-file)]))

  (match (mode)
        ['dry (log (format "*~a" action-str))]
        ['build
         (log action-str)
         (when exec?
           (system/exit-code command))]
        ['check
         (if (target-is-older? target-file src-files)
             (log "ERROR (target is older):" #:func eprintf)
             (log "Checking"))]
        [m (error "Wrong mode: ~v" m)])

  (result-ok deps))
