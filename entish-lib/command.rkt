#lang racket/base
(require (only-in racket/cmdline
                  command-line)
         (only-in racket/vector
                  vector-drop)
         entish
         raco/command-name
         compiler/embed
         compiler/distribute)

(provide (all-defined-out))

(define-namespace-anchor a)

(module+ raco
  (define command-name (with-handlers ([exn:fail? (lambda _ #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(module+ main
  (define command-name (with-handlers ([exn:fail? (lambda _ #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(define (dispatch command-name)
  (case command-name
    [(#f "help") (handle-help)]
    [("build" "check" "dry") (handle-command (string->symbol command-name))]
    [("compile") (handle-compile)]))

(define (handle-help)
  (displayln (format "

entish: a language for directory tree manipulation

Available commands:

help - show this message
build - build the directory tree
check - check the directory tree
compile - compile script into standalone executable

")))

(define (run-command cmd args)

  (parameterize ([mode cmd]
                 [current-namespace (namespace-anchor->namespace a)])
    (namespace-require 'entish)

    ;; Here we could also load user-provided functions...
    (for ([f args])
      (load f))))

(define (subcommand-argv) (vector-drop (current-command-line-arguments) 1))
(define (handle-command cmd)

  (define file-list
    (command-line #:program "raco entish build"
                  #:argv (subcommand-argv)
                  #:args args args))

  (run-command cmd file-list))

(define (handle-compile)
  (define-values (exe-name file-list) 
    (command-line #:program "raco entish compile"
                  #:argv (subcommand-argv)
                  #:args (exe-name . file-list) (values exe-name file-list)))

  (displayln (list exe-name file-list))
  (create-embedding-executable exe-name
                               #:modules '(;(#f "entish-lib/main.rkt")
                                           (launcher "entish-lib/launcher.rkt"))
                               #:collects-path (current-library-collection-paths)
                               #:configure-via-first-module? #t
                               #:cmdline '("--")

                               #:literal-expressions ;'((require command))
                               (parameterize ([current-namespace (namespace-anchor->namespace a)
                                                                ; (make-base-namespace)
                                                                 ])
                                 (list
;                                  '(require 'entish/command)
                                  (compile `(namespace-require 'entish))
                                  (compile `(namespace-require 'entish/launcher))
                                  (compile `(handle-help))
                                  ))
                               ;; #:aux '((ico . "db_backup.ico"))
                               #:verbose? #t
                               #:gracket? #f)
  )
#|

  ;; Compilation
  (create-embedding-executable *EXE-NAME*
                               #:modules '((#f "badpixels.rkt"))
                               #:collects-path (current-library-collection-paths)
                               #:configure-via-first-module? #t
                               #:cmdline '("--")

                               #:literal-expression
                               (parameterize ([current-namespace (make-base-namespace)])
                                 (compile `(namespace-require ''badpixels)))
                               ;; #:aux '((ico . "db_backup.ico"))
                               #:verbose? #t
                               #:gracket? #f
                               #:launcher? #f)
;; Packaging
  (assemble-distribution *DEPLOY-DIR* (list *EXE-NAME*)))

;;;;;;;;;;
;; MAIN ;;
;;;;;;;;;;

(define (running-standalone?)
    (not
        (member (path->string (file-name-from-path (find-system-path 'run-file)))
                           '("DrRacket.exe" "run.rkt"))))
|#
