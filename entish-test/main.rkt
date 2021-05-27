#lang racket
(require entish
         rackunit)

(define (load-test fname)
  (let-values ([(base f whatever)
                (split-path (resolved-module-path-name
                             (variable-reference->resolved-module-path
                              (#%variable-reference))))])
             (load (build-path base fname))))

(define-namespace-anchor a)

(test-case "Simple test case"
  (test-begin
    (parameterize [(mode 'dry)
                   (overwrite-mode 'skip)
                   [current-namespace (namespace-anchor->namespace a)]
                   ]

      (namespace-require 'entish)
      ((load-test  "simple.rkt"))
      ((load-test  "node.rkt")))

    (check-equal?
     (file->string (build-path (find-system-path 'temp-dir) "prova.txt"))
     "PrOva")))

