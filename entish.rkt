#lang racket
(require (for-syntax syntax/keyword
                     racket/list
                     racket/match
                     racket/port)
         racket/generator)

(provide dir
         file
         root)

;;; SYNTAX
(define-for-syntax (build-name-stx stx . parts)  
  (datum->syntax stx
                 (string->symbol 
                  (with-output-to-string 
                   (lambda () 
                     (for ([p parts])
                       (display (if (syntax? p) 
                                    (syntax-e p) 
                                    p))))))))

(define-for-syntax (syntax->keyword syn)
  (string->keyword (symbol->string (syntax-e syn))))

(define-syntax (defnode stx)
  (define (build-name . parts)
    (apply build-name-stx (cons stx parts)))
  
  (syntax-case stx ()
    ([defnode (name ((k t) ... )) body ...]
     (with-syntax ([fname (build-name #'name "-func")]
                   [children (datum->syntax stx 'children)]
                   [kws-pairs (foldl (lambda (cur acum)
                                       (cons (syntax->keyword cur)
                                             (cons (list cur #f) 
                                                   acum)))
                                     '()
                                     (syntax->list #'(k ...)))]
                   [kws-table (cons 'list
                                    (for/list ([kw-syn (syntax->list #'(k ...))]
                                               [type-syn (syntax->list #'(t ...))])
                                      `(list ',(syntax->keyword kw-syn)
                                             ,(build-name #'check- type-syn))))])
       #`(begin
           (define-syntax (name stx)
             (syntax-case stx ()
               ([_ . rest ] (let-values ([(kw nokw) (parse-keyword-options 
                                                     #'rest
                                                     kws-table)])
                              
                              (define syn-kw (map cdr kw)) 
                              #`(fname #,@(flatten syn-kw) #,@nokw)))))
           (define (fname #,@#'kws-pairs . children)
             (lambda ()
               (bind-aggregate (lambda () (call-with-values
                                         (lambda () 
                                           (call/cc (lambda (#,(datum->syntax stx 'return))
                                                      body ... )))
                                       list))
                               '()))))))))

(define-syntax (fetch stx)
  (syntax-case stx ()
    ([_ id] (with-syntax ([name (build-name-stx stx #'state- #'id)])
              #'(name (current-state))))))

(define-syntax (with-new-state-frame stx)
  (syntax-case stx ()
    ([_ ((k v) ...) body ...] 
     #'(parameterize ([current-state (struct-copy state (current-state) (k v) ...)])
         body ...))))

(define-syntax-rule (case-mode (k exp ... ) ...) 
  (cond ([equal? 'k (fetch mode)] exp ... ) ... ))

(define-syntax (maybe-loop-with stx)
  (syntax-case stx ()
    [(maybe-loop-with body ... )
     (with-syntax ([generator (datum->syntax stx 'generator)]
                   [vals (datum->syntax stx 'values)]
                   [return (datum->syntax stx 'return)]
                   [name (datum->syntax stx 'name)])
       #'(if generator
             (let loop ([acum '()])
               (call-with-values (lambda () (generator))
                 (lambda vals
                   (if (not (eof-object? (car vals)))
                     (let ([name (interpolate name vals)])
                       (loop
                        (call-with-values
                         (lambda ()
                           (call/cc (lambda (return)
                                      body ...
                                      '())))
                         (match-lambda*
                           ['(()) acum]
                           [(list (? list? x))  (append x acum)]
                           [(? list y) (cons y acum)]))))
                     ;else
                     acum))))
             (begin
               body ... '())))]))

;;; STATE
(define-struct state 
  (base-path mode)
  #:transparent)

(define current-state 
  (make-parameter (make-state "." 'test)))

;;; AUX FUNCS
(define (interpolate str vals)
  (regexp-replace*  #px"\\?([0-9]+)"
                    str
                    (lambda (all capture)
                      (format "~a"
                              (list-ref vals (string->number capture))))))

(define bind-aggregate
  (lambda (func acum)
    (let ([val (func)])
      (match val
        ['() acum]
        [(list (? list? x)) (append x acum)]
        [(list (? list? x) ... ) (append x acum)]
        [(list y ...) (cons y acum)]))))

(define (aggregate children)
  ;; Fer amb un fold.
  (foldl bind-aggregate
   '()
   children))

;;; NODES
(defnode (file ([name expression]
                [generator expression]))
     (maybe-loop-with
      (let ([file-path (build-path (fetch base-path) name)])
        (case-mode 
         [test ;; In test mode, check content matches regexp
          (or (file-exists? file-path)
              (return 'file-does-not-exist file-path))
          (or (regexp-match (pregexp (apply string-append children))
                            (file->string file-path))
              (return 'file-contents-mismatch file-path))]
         [build
          (display-to-file (apply string-append children)
                           file-path
                           #:exists 'truncate/replace)]
         [remove
          (when (file-exists? file-path)
            (delete-file file-path))]))))

(defnode (dir ([name expression]
               [generator expression]))
  (maybe-loop-with
   (let ([full-path (build-path (fetch base-path) name)])
     (case-mode
      [test
       (or (directory-exists? full-path)
           (return 'directory-does-not-exist full-path))]
      (return
       (with-new-state-frame ([base-path full-path])
                             (aggregate children)))      
      [build
       (unless (directory-exists? full-path)
         (make-directory full-path))]
      [remove
       (with-new-state-frame ([base-path full-path])
                             (aggregate children))
       (when (and (directory-exists? full-path)
                  (null? (directory-list full-path))) 
         (delete-directory full-path))]))))

(define (root-func #:path [path "."]
                   #:mode [mode 'test]
                   . children)
  (with-new-state-frame ([base-path path]
                         [mode mode])
                        (aggregate children)))

(define-syntax (root stx)
  (syntax-case stx ()
    ([_ . rest ] (let-values ([(kw nokw) (parse-keyword-options 
                                          #'rest
                                          (list (list '#:path check-expression)
                                                (list '#:mode check-expression)) )])
                   (define syn-kw (map cdr kw)) 
                   #`(root-func #,@(flatten syn-kw) #,@nokw)))))
