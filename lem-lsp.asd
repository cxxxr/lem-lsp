(asdf:defsystem lem-lsp
  :depends-on (:lem :jsonrpc :alexandria :lsp-client)
  :serial t
  :components ((:file "lem-lsp")))
