(cl:defpackage #:lsp-protocol
  (:use #:cl))
(in-package #:lsp-protocol)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun optionalp (sym)
    (let ((str (string sym)))
      (when (char= #\? (char str (1- (length str))))
        (values
         (intern (subseq str 0 (1- (length str)))
                 (symbol-package sym))))))

  (defun yb (sym)
    (intern (format nil "_~A" sym))))

(defmacro define-interface (class-name (&optional parent) &rest slots)
  `(defclass ,(yb class-name)
       ,(if (null parent)
            '()
            `(,(yb parent)))
     ,(mapcar (lambda (slot)
                (destructuring-bind (slot-name type) slot
                  (let ((sym (optionalp slot-name)))
                    (if sym
                        `(,sym
                          :initarg ,(intern (string-upcase sym) :keyword)
                          :initform nil
                          :reader ,(intern (format nil "~A-~A" class-name sym))
                          :type ,type)
                        `(,slot-name
                          :initarg ,(intern (string-upcase slot-name) :keyword)
                          ;:initform (error ,(format nil "slot ~A unbound" slot-name))
                          :reader ,(intern (format nil "~A-~A" class-name slot-name))
                          :type ,type)))))
              slots)))

(define-interface Position ()
  (line integer)
  (character integer))

(define-interface Range ()
  (start Position)
  (end Position))

(define-interface Location ()
  (uri string)
  (range Range))

(define-interface Diagnostic ()
  (range Range)
  (severity? integer)
  (code? (or integer string))
  (source? string)
  (message string))

(define-interface Command ()
  (title string)
  (command string)
  (arguments? list))

(define-interface TextEdit ()
  (range Range)
  (newText string))

(define-interface WorkspaceEdit ()
  (changes t) ; { [uri: string]: TextEdit[]; };
  )

(define-interface TextDocumentIdentifier ()
  (uri string))

(define-interface TextDocumentItem ()
  (uri string)
  (languageId string)
  (version integer)
  (text string))

(define-interface VersionedTextDocumentIdentifier (TextDocumentIdentifier)
  (version integer))

(define-interface TextDocumentPositionParams ()
  (textDocument TextDocumentIdentifier)
  (position Position))

(define-interface DocumentFilter ()
  (language? string)
  (scheme? string)
  (pattern? string))

(define-interface InitializeParams ()
  (processId (or integer null))
  (rootPath (or string null))
  (rootUri (or string null))
  (initializationOptions? t)
  (capabilities ClientCapabilities)
  (trace? (member :off :messages :verbose)))
