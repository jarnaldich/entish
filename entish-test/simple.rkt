(forest (roots (tmp (find-system-path 'temp-dir)))

        (tmp
         (dir "src"

              (file* "prova.txt"
                     (template "PrOva"))

              #;(file* (sequence->generator (list "prova1.txt" "prova2.txt"))
                    (template "Prova"))
              (file "prova1.txt"
                    (template-string "PrOva"))
              (file "prova2.txt"
                    (template-string "PrOva")))
         (dir "dst"
              (+seq+
               ;               (delete "*.txt")
               (copy-from tmp "src" "prova*.txt" #:match #rx"prova" #:replace "manuela")
               (file "copy_of_prova.txt"
                     (copy-from tmp "src" "prova.txt"))))))
