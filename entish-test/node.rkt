(forest (roots (tmp (find-system-path 'temp-dir)))
        (tmp
         (dir "src"
              (dummy "breadcrumb-component" "param1" "paramd"
                     (dir "other"))
              (dummy (sequence->generator (list "1st" "2nd" "3rd")) "param1" "param2"
                     (dir "other2"))
              (file "patata.txt"
                    (dummy-content "uno" "dos")))))
