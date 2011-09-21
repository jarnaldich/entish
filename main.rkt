#!/usr/bin/env racket
#lang racket
(require "entish.rkt"
         racket/generator
         racket/cmdline)

;;; Params
(define root-path (make-parameter "."))
(define mode (make-parameter 'test))
(define rest-args (make-parameter '()))
(define spec-file (make-parameter "tree.rkt"))

;;; Command-line parsing
(define extra-args
  (command-line
   #:program "entish"
   #:once-each
   [("-r" "--root")
    path
    "Root Path. Default is current path."
    (root-path path)]
   [("-f" "--file")
    fname
    "Tree spec file name (default is tree.rkt)."
    (spec-file fname)]
   #:once-any
   [("-t" "--test")
    "Test mode (checks if the spec is met). This is the default."
    (mode 'test)]
   [("-b" "--build")
    "Build mode (creates the directory structure)."
    (mode 'build)]
   [("-r" "--remove")
    "Remove mode. Deletes the directory structure."
    (mode 'remove)]
   #:args args
   (rest-args args)))

(define (humanize-result res)
  (if (null? res)
      (displayln "OK")
      (for-each (match-lambda
                 [(list 'file-does-not-exist f)
                  (printf "ERROR: File ~a does not exist.~n"
                          (path->string f))]
                 [(list 'directory-does-not-exist f)
                  (printf "ERROR: Directory ~a does not exist.~n"
                          (path->string f))])
               res)))


;;; MAIN
(with-new-state-frame
 ([base-path (root-path)]
  [mode (mode)])
 (parameterize ([current-namespace (make-base-namespace)])
   (namespace-require "entish.rkt")
   (namespace-require 'racket/generator)
   (humanize-result (load (spec-file)))))
