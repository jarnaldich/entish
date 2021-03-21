;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recipe
(forest (roots src-dir ; Agafa de variable d'entorn o param
               (stage-dir "e:\\temp")
               (datacloud_2 "\\\\icgc\\dades\\datacloud_2"))


         (datacloud_2 (dir "tif_unzip"
                           (zip "patata.txt")
                           (file "hola.txt"
                                 (copy-from stage-dir "arxiu.txt"))
                           (file "hoxla.txt"
                                 (template-string "asdasdfasdf"))))

         (stage-dir (file "prova.txt")))
