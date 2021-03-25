#lang info

(define collection "entish")
(define pkg-desc "Entish is a language for describing directory trees")
(define version "0.1")

(define deps '("base"
               ["racket" "6.0"]))

(define build-deps '("rackunit-lib"))
(define package-authors '("Joan Arnaldich <guesswhat@donotevercontactme.com>"))
;; Add scribblings, dependencies, build-dependencies if needed...

(define raco-commands
  '(("entish" (submod argo/command raco)
              "Scaffold directory trees"
              #f)))
