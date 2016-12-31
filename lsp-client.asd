(asdf:defsystem lsp-client
  :depends-on (#:closer-mop)
  :components ((:module "lsp-client"
                :serial t
                :components ((:file "protocol")))))
