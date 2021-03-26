;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recipe
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
              (file "copy_of_prova.txt"
                    (copy-from tmp "prova.txt")))))

