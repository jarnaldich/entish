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
    [("build" "check" "dry") (handle-command (string->symbol command-name))]))

(define (handle-help)
  (displayln (format "

entish: a language for directory tree manipulation

Available commands:

help - show this message
build - build the directory tree
check - check the directory tree

")))

(define (run-command cmd ovrwrite-mode args)

  (parameterize ([mode cmd]
                 [overwrite-mode ovrwrite-mode]
                 [current-namespace (namespace-anchor->namespace a)])
    (namespace-require 'entish)

    ;; Here we could also load user-provided functions...
    (for ([f args])
      ((load f)))))


(define (subcommand-argv) (vector-drop (current-command-line-arguments) 1))
(define (handle-command cmd)

  (define overwrite-mode (make-parameter 'fail)) ; fail | overwrite | skip

  (define file-list
    (command-line #:program "raco entish build"
                  #:argv (subcommand-argv)
                  #:once-any
                  [("-o" "--overwrite") "Overwrite existing files or data"
                                        (overwrite-mode 'overwrite)]
                  [("-s" "--skip") "Skip (do not overwrite or fail) pre-existing file or data"
                                   (overwrite-mode 'skip)]
                  [("-n" "--newer") "Overwrite only if newer file or data"
                                   (overwrite-mode 'newer)]
                  #:args args args))

  (run-command cmd (overwrite-mode) file-list))
