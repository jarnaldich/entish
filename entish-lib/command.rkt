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
dry   - perform a dry run (do not modify anything)

")))

(define (run-command cmd args)

  (parameterize ([mode cmd]
                 [current-namespace (namespace-anchor->namespace a)])
    (namespace-require 'entish)

    ;; Here we could also load user-provided functions...
    (for ([f args])
      ((load f)))))

(define (subcommand-argv) (vector-drop (current-command-line-arguments) 1))
(define (handle-command cmd)

  (define file-list
    (command-line #:program "raco entish build"
                  #:argv (subcommand-argv)
                  #:args args args))

  (run-command cmd file-list))
