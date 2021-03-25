#lang racket
(require entish
         rackunit)

(test-case "Simple test case"
  (test-begin
    (parameterize [(mode 'build)]
      (forest (roots (tmp (find-system-path 'temp-dir)))

              (tmp
               (dir "src"
                    (file "prova.txt"
                          (template-string "PrOva"))
                    (file "prova1.txt"
                          (template-string "PrOva"))
                    (file "prova1.txt"
                          (template-string "PrOva")))
               (dir "dst"
                    (clear)
                    (file "copy_of_prova.txt"
                          (copy-from tmp "prova.txt"))))))

    (check-equal?
     (file->string (build-path (find-system-path 'temp-dir) "prova.txt"))
     "PrOva")))
