;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recipe

(forest (roots
         (datacloud_2 "\\\\icgc\\dades\\datacloud_2\\bseccen_ETRS89")
         (datacloud "\\\\datacloud.icgc.cat\\datacloud\\bseccen_ETRS89")
               (datacloud_cifs "\\\\datacloud.icgc.cat\\datacloud_cifs\\bseccen_ETRS89"))

        (datacloud (dir "shp"
                        (copy-from datacloud_2 "shp" "*.zip")))

        (datacloud_cifs (dir "shp_unzip"
                             (copy-from datacloud_2 "shp_unzip_cifs" "*.*"))))
