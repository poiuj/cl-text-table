(defsystem cl-text-table
           :description "Library for printing text tables"
           :version "1.0"
           :author "poiuj"
           :components ((:static-file "package.lsp")
                        (:static-file "text-table.lsp"
                                      :depends-on 
                                      ("package.lsp"))))
