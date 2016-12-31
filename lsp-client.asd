(asdf:defsystem lsp-client
  :depends-on ()
  :components ((:module "lsp-client"
                :serial t
                :components ((:file "protocol")))))
