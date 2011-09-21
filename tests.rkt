#lang racket
(require "entish.rkt"
         rackunit
         rackunit/text-ui         
         racket/generator)

(define-syntax-rule (sample-tree-mode mode) 
  (root #:path (find-system-path 'temp-dir)
        #:mode mode
        (dir #:name "?0"
             #:foreach (sequence->generator '("OneDir"
                                              "TwoDirs"
                                              "ThreeDirs")))
        (file #:name "?0.?1"
              #:foreach (generator ()
                                   (for ([i (in-range 3)])
                                        (yield i "txt"))
                                   (yield eof))
              "Sample text")))

(test-case
 "Build and test should do the same"
 (check-equal? (sample-tree-mode 'remove) '())
 (check-equal? (length (sample-tree-mode 'test))  6)
 (check-equal? (sample-tree-mode 'build) '())
 (check-equal? (sample-tree-mode 'test) '())
 (check-equal? (sample-tree-mode 'remove) '()))


