(asdf:defsystem lsp-client
  :depends-on (:lem :jsonrpc :alexandria)
  :serial t
  :components ((:file "lsp")
               ))
