#!/usr/bin/env racket
#lang racket

;;; Params
(define root-path (make-parameter "."))
(define mode (make-parameter 'test))
(define rest-args (make-parameter '()))
(define spec-file (make-parameter "tree.rkt"))

;;; Command-line parsing
;;TODO: Add verbosity
;;TODO: Add
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

;;; MAIN
(parameterize ([current-namespace (make-base-namespace)])
  (namespace-require "./entish/macros.rkt")
  (namespace-require "./entish/builtins.rkt")
  ;; Here we could also load user-provided functions...
  (load (spec-file)))
