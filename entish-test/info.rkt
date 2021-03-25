#lang info
(define collection 'tests/entish)
(define deps '("base"))
(define pkg-desc "Tests for entish")
(define pkg-authors '(jarnaldich))
(define build-deps '("rackunit-lib"))
(define update-implies '("entish-lib"))
