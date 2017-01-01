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

(define-interface |TextDocumentClientCapabilities| ()
  )
