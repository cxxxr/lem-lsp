(cl:defpackage #:lsp-protocol
  (:use #:cl)
  (:export #:interface))
(in-package #:lsp-protocol)

(defun remove-properties (plist keys)
  (let ((acc '()))
    (loop :for (k v) :on plist :by #'cddr
          :do (unless (member k keys)
                (push k acc)
                (push v acc)))
    (nreverse acc)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkstr (&rest args)
    (with-output-to-string (s)
      (dolist (a args) (princ a s))))

  (defun symb (&rest args)
    (values (intern (apply #'mkstr args))))

  (defun optionalp (sym)
    (let ((str (string sym)))
      (when (char= #\? (char str (1- (length str))))
        (values
         (intern (subseq str 0 (1- (length str)))
                 (symbol-package sym))))))

  (defun to-keyword (sym)
    (intern (string sym) :keyword))

  (defun to-var (sym)
    (setf sym (to-keyword sym))
    (or (optionalp sym)
        sym))

  (defun gen-check-name (name)
    (symb 'check- name))

  (defun gen-check-1 (definitions)
    (let ((g-default-value (gensym)))
      `(progn
         (let ((unknown-plist (remove-properties plist ',(mapcar #'to-var (mapcar #'car definitions)))))
           (when unknown-plist
             (loop :for (k v) :on unknown-plist :by #'cddr
                   :do (warn "unexpected elements: ~A ~A" k v))))
         ,@(loop :for (var type) :in definitions
                 :for optionalp := (optionalp var)
                 :do (when optionalp (setf var optionalp))
                 :do (setf var (to-keyword var))
                 :collect `(let ((value (getf plist ,var ',g-default-value)))
                             ,@(unless optionalp
                                 `((when (eq ',g-default-value value)
                                     (error ,(format nil "require indicator: ~A" var)))))
                             (unless (eq ',g-default-value value)
                               ,(if (and (consp type) (eq 'json (car type)))
                                    `(let ((plist value)) ,(gen-check-1 (cdr type)))
                                    (if (and (symbolp type) (get type 'interface-type))
                                        `(,(symb 'check- type) value)
                                        `(check-type value ,type)))))))))

  (defun gen-check-type (name parents definitions)
    `(defun ,(gen-check-name name) (object)
       (when (trivial-types:property-list-p object)
         ,@(mapcar (lambda (parent)
                     `(,(gen-check-name parent) object))
                   parents)
         (let ((plist object))
           ,(gen-check-1 definitions))))))

(defun interface (name &rest plist)
  (funcall (get name 'check-fn) plist)
  (let ((hashtable (make-hash-table)))
    (loop :for (k v) :on plist :by #'cddr
          :do (setf (gethash (string k) hashtable) v))
    hashtable))

(defmacro define-interface (class-name (&rest parents) &rest definitions)
  `(progn
     (setf (get ',class-name 'interface-type) t)
     (setf (get ',class-name 'check-fn)
           ,(gen-check-type class-name parents definitions))))

(define-interface |Position| ()
  (line integer)
  (character integer))

(define-interface |Range| ()
  (start |Position|)
  (end |Position|))

(define-interface |Location| ()
  (uri string)
  (range |Range|))

(define-interface |Diagnostic| ()
  (range |Range|)
  (severity? integer)
  (code? (or integer string))
  (source? string)
  (message string))

(define-interface |Command| ()
  (title string)
  (command string)
  (arguments? list))

(define-interface |TextEdit| ()
  (range |Range|)
  (newText string))

(define-interface |WorkspaceEdit| ()
  (changes t) ; { [uri: string]: TextEdit[]; };
  )

(define-interface |TextDocumentIdentifier| ()
  (uri string))

(define-interface |TextDocumentItem| ()
  (uri string)
  (languageId string)
  (version integer)
  (text string))

(define-interface |VersionedTextDocumentIdentifier| (|TextDocumentIdentifier|)
  (version integer))

(define-interface |TextDocumentPositionParams| ()
  (textDocument |TextDocumentIdentifier|)
  (position |Position|))

(define-interface |DocumentFilter| ()
  (language? string)
  (scheme? string)
  (pattern? string))

(define-interface |InitializeParams| ()
  (processId (or integer null))
  (rootPath (or string null))
  (rootUri (or string null))
  (initializationOptions? t)
  (capabilities |ClientCapabilities|)
  (trace? (member :off :messages :verbose)))

(define-interface |WorkspaceClientCapabilities| ()
  (applyEdit? boolean)
  (didChangeConfiguration? (json (dynamicRegistration? boolean)))
  (didChangeWatchedFiles? (json (dynamicRegistration? boolean)))
  (symbol? (json (dynamicRegistration? boolean)))
  (executeCommand? (json (dynamicRegistration? boolean))))

(define-interface |TextDocumentClientCapabilities| nil
 (synchronization?
  (json (dynamicregistration? boolean) (willsave? boolean)
   (willsavewaituntil? boolean) (didsave? boolean)))
 (completion?
  (json (dynamicregistration? boolean)
   (completionitem? (json (rangeproperty? boolean) (typedstring? boolean)))))
 (hover? (json (dynamicregistration? boolean)))
 (signaturehelp? (json (dynamicregistration? boolean)))
 (references? (json (dynamicregistration? boolean)))
 (documenthighlight? (json (dynamicregistration? boolean)))
 (documentsymbol? (json (dynamicregistration? boolean)))
 (formatting? (json (dynamicregistration? boolean)))
 (rangeformatting? (json (dynamicregistration? boolean)))
 (ontypeformatting? (json (dynamicregistration? boolean)))
 (definition? (json (dynamicregistration? boolean)))
 (codeaction? (json (dynamicregistration? boolean)))
 (codelens? (json (dynamicregistration? boolean)))
 (documentlink? (json (dynamicregistration? boolean)))
 (rename? (json (dynamicregistration? boolean))))

(define-interface |ClientCapabilities| nil
 (workspace? |WorkspaceClientCapabilites|)
 (textdocument? |TextDocumentClientCapabilities|) (experimental? t))

(define-interface |InitializeResult| nil (capabilities |ServerCapabilities|))

(progn (defparameter |unknownProtocolVersion| 1))

(define-interface |InitializeError| nil (retry boolean))

(progn
 (defparameter |None| 0)
 (defparameter |Full| 1)
 (defparameter |Incremental| 2))

(define-interface |CompletionOptions| nil (resolveprovider? boolean)
 (triggercharacters? (trivial-types:proper-list string)))

(define-interface |SignatureHelpOptions| nil
 (triggercharacters? (trivial-types:proper-list string)))

(define-interface |CodeLensOptions| nil (resolveprovider? boolean))

(define-interface |DocumentOnTypeFormattingOptions| nil
 (firsttriggercharacter string)
 (moretriggercharacter? (trivial-types:proper-list string)))

(define-interface |DocumentLinkOptions| nil (resolveprovider? boolean))

(define-interface |ExecuteCommandOptions| nil
 (commands (trivial-types:proper-list string)))

(define-interface |SaveOptions| nil (includetext? boolean))

(define-interface |TextDocumentSyncOptions| nil (openclose? boolean)
 (change? integer) (willsave? boolean) (willsavewaituntil? boolean)
 (save? |SaveOptions|))

(define-interface |ServerCapabilities| nil
 (textdocumentsync? (or |TextDocumentSyncOptions| integer))
 (hoverprovider? boolean) (completionprovider? |CompletionOptions|)
 (signaturehelpprovider? |SignatureHelpOptions|) (definitionprovider? boolean)
 (referencesprovider? boolean) (documenthighlightprovider? boolean)
 (documentsymbolprovider? boolean) (workspacesymbolprovider? boolean)
 (codeactionprovider? boolean) (codelensprovider? |CodeLensOptions|)
 (documentformattingprovider? boolean)
 (documentrangeformattingprovider? boolean)
 (documentontypeformattingprovider? |DocumentOnTypeFormattingOptions|)
 (renameprovider? boolean) (documentlinkprovider? |DocumentLinkOptions|)
 (executecommandprovider? |ExecuteCommandOptions|))

(define-interface |ShowMessageParams| nil (type integer) (message string))

(progn
 (defparameter |Error| 1)
 (defparameter |Warning| 2)
 (defparameter |Info| 3)
 (defparameter |Log| 4))

(define-interface |ShowMessageRequestParams| nil (type integer)
 (message string) (actions? (trivial-types:proper-list |MessageActionItem|)))

(define-interface |MessageActionItem| nil (title string))

(define-interface |LogMessageParams| nil (type integer) (message string))

(define-interface |Registration| nil (id string) (method string)
 (registeroptions? t))

(define-interface |RegistrationParams| nil
 (registrations (trivial-types:proper-list |Registration|)))

(define-interface |TextDocumentRegistrationOptions| nil
 (documentselector (or |DocumentSelector| |null|)))

(define-interface |Unregistration| nil (id string) (method string))

(define-interface |UnregistrationParams| nil
 (unregisterations (trivial-types:proper-list |Unregistration|)))

(define-interface |DidChangeConfigurationParams| nil (settings t))

(define-interface |DidOpenTextDocumentParams| nil
 (textdocument |TextDocumentItem|))

(define-interface |DidChangeTextDocumentParams| nil
 (textdocument |VersionedTextDocumentIdentifier|)
 (contentchanges (trivial-types:proper-list |TextDocumentContentChangeEvent|)))

(define-interface |TextDocumentContentChangeEvent| nil (range? |Range|)
 (rangelength? integer) (text string))

(define-interface |TextDocumentChangeRegistrationOptions|
 (|TextDocumentRegistrationOptions|) (synckind integer))

(define-interface |WillSaveTextDocumentParams| nil
 (textdocument |TextDocumentIdentifier|) (reason integer))

(progn
 (defparameter |Manual| 1)
 (defparameter |AfterDelay| 2)
 (defparameter |FocusOut| 3))

(define-interface |DidSaveTextDocumentParams| nil
 (textdocument |TextDocumentIdentifier|) (text? string))

(define-interface |TextDocumentSaveRegistrationOptions|
 (|TextDocumentRegistrationOptions|) (includetext? boolean))

(define-interface |DidCloseTextDocumentParams| nil
 (textdocument |TextDocumentIdentifier|))

(define-interface |DidChangeWatchedFilesParams| nil
 (changes (trivial-types:proper-list |FileEvent|)))

(define-interface |FileEvent| nil (uri string) (type integer))

(progn
 (defparameter |Created| 1)
 (defparameter |Changed| 2)
 (defparameter |Deleted| 3))

(define-interface |PublishDiagnosticsParams| nil (uri string)
 (diagnostics (trivial-types:proper-list |Diagnostic|)))

(define-interface |CompletionList| nil (isincomplete boolean)
 (items (trivial-types:proper-list |CompletionItem|)))

(define-interface |CompletionItem| nil (label string) (kind? integer)
 (detail? string) (documentation? string) (sorttext? string)
 (filtertext? string) (inserttext? string) (textedit? |TextEdit|)
 (additionaltextedits? (trivial-types:proper-list |TextEdit|))
 (command? |Command|) (data? t))

(progn
 (defparameter |Text| 1)
 (defparameter |Method| 2)
 (defparameter |Function| 3)
 (defparameter |Constructor| 4)
 (defparameter |Field| 5)
 (defparameter |Variable| 6)
 (defparameter |Class| 7)
 (defparameter |Interface| 8)
 (defparameter |Module| 9)
 (defparameter |Property| 10)
 (defparameter |Unit| 11)
 (defparameter |Value| 12)
 (defparameter |Enum| 13)
 (defparameter |Keyword| 14)
 (defparameter |Snippet| 15)
 (defparameter |Color| 16)
 (defparameter |File| 17)
 (defparameter |Reference| 1))

(define-interface |CompletionRegistrationOptions|
 (|TextDocumentRegistrationOptions|)
 (triggercharacters? (trivial-types:proper-list string))
 (resolveprovider? boolean))

(define-interface |Hover| nil
 (contents (or |MarkedString| (trivial-types:proper-list |MarkedString|)))
 (range? |Range|))

(define-interface |SignatureHelp| nil
 (signatures (trivial-types:proper-list |SignatureInformation|))
 (activesignature? integer) (activeparameter? integer))

(define-interface |SignatureInformation| nil (label string)
 (documentation? string)
 (parameters? (trivial-types:proper-list |ParameterInformation|)))

(define-interface |ParameterInformation| nil (label string)
 (documentation? string))

(define-interface |SignatureHelpRegistrationOptions| (|TextDocumentRegistrationOptions|)
  (triggercharacters? (trivial-types:proper-list string)))

(define-interface |ReferenceParams| (|TextDocumentPositionParams|)
  (context |ReferenceContext|))

(define-interface |ReferenceContext| nil (includedeclaration boolean))

(define-interface |DocumentHighlight| nil (range |Range|) (kind? integer))

(progn
 (defparameter |Text| 1)
 (defparameter |Read| 2)
 (defparameter |Write| 3))

(define-interface |DocumentSymbolParams| nil
 (textdocument |TextDocumentIdentifier|))

(define-interface |SymbolInformation| nil (name string) (kind integer)
 (location |Location|) (containername? string))

(progn
 (defparameter |File| 1)
 (defparameter |Module| 2)
 (defparameter |Namespace| 3)
 (defparameter |Package| 4)
 (defparameter |Class| 5)
 (defparameter |Method| 6)
 (defparameter |Property| 7)
 (defparameter |Field| 8)
 (defparameter |Constructor| 9)
 (defparameter |Enum| 10)
 (defparameter |Interface| 11)
 (defparameter |Function| 12)
 (defparameter |Variable| 13)
 (defparameter |Constant| 14)
 (defparameter |String| 15)
 (defparameter |Number| 16)
 (defparameter |Boolean| 17)
 (defparameter |Array| 18))

(define-interface |WorkspaceSymbolParams| nil (query string))

(define-interface |CodeActionParams| nil
 (textdocument |TextDocumentIdentifier|) (range |Range|)
 (context |CodeActionContext|))
(define-interface |CodeActionContext| nil
 (diagnostics (trivial-types:proper-list |Diagnostic|)))

(define-interface |CodeLensParams| nil (textdocument |TextDocumentIdentifier|))

(define-interface |CodeLens| nil (range |Range|) (command? |Command|) (data? t))

(define-interface |CodeLensRegistrationOptions|
 (|TextDocumentRegistrationOptions|) (resolveprovider? boolean))

(define-interface |DocumentLinkParams| nil
 (textdocument |TextDocumentIdentifier|))
(define-interface |DocumentLink| nil (range |Range|) (target string))

(define-interface |DocumentLinkRegistrationOptions|
 (|TextDocumentRegistrationOptions|) (resolveprovider? boolean))

(define-interface |DocumentFormattingParams| nil
 (textdocument |TextDocumentIdentifier|) (options |FormattingOptions|))

;; (define-interface |FormattingOptions| nil
;;   )

(define-interface |DocumentRangeFormattingParams| nil
 (textdocument |TextDocumentIdentifier|) (range |Range|)
 (options |FormattingOptions|))

(define-interface |DocumentOnTypeFormattingParams| nil
 (textdocument |TextDocumentIdentifier|) (position |Position|) (ch string)
 (options |FormattingOptions|))

(define-interface |DocumentOnTypeFormattingRegistrationOptions|
 (|TextDocumentRegistrationOptions|) (firsttriggercharacter string)
 (moretriggercharacter? (trivial-types:proper-list string)))

(define-interface |RenameParams| nil (textdocument |TextDocumentIdentifier|)
 (position |Position|) (newname string))

(define-interface |ExecuteCommandParams| nil (command string)
 (arguments? (trivial-types:proper-list t)))

(define-interface |ExecuteCommandRegistrationOptions| nil
 (commands (trivial-types:proper-list string)))

(define-interface |ApplyWorkspaceEditParams| nil (edit |WorkspaceEdit|))
(define-interface |ApplyWorkspaceEditResponse| nil (applied boolean))
