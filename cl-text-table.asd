(defsystem cl-text-table
  :description "Library for printing text tables"
  :version "1.0"
  :author "poiuj"
  :components ((:file "package" :type "lsp")
               (:file "text-table" :type "lsp"
                      :depends-on 
                      ("package"))))
