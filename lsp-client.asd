(asdf:defsystem lsp-client
  :depends-on (:trivial-types)
  :components ((:module "lsp-client"
                :serial t
                :components ((:file "protocol")))))
