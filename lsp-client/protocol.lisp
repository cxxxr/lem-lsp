(cl:defpackage #:lsp-protocol
  (:use #:cl)
  (:export #:to-hashtable))
(in-package #:lsp-protocol)

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
                 (symbol-package sym)))))))

(defmacro define-interface (class-name parent &rest slots)
  `(defclass ,class-name ,parent
     ,(mapcar (lambda (slot)
                (destructuring-bind (slot-name type) slot
                  (let ((sym (optionalp slot-name)))
                    (if sym
                        `(,sym
                          :initarg ,(intern (string-upcase sym) :keyword)
                          :initform nil
                          :reader ,(symb sym '-of)
                          :type ,type)
                        `(,slot-name
                          :initarg ,(intern (string-upcase slot-name) :keyword)
                          ;:initform (error ,(format nil "slot ~A unbound" slot-name))
                          :reader ,(symb slot-name '-of)
                          :type ,type)))))
              slots)))

(defun to-hashtable (x)
  (let ((table (make-hash-table :test 'equal)))
    (loop :for slot :in (c2mop:class-slots (class-of x))
          :for slot-name := (c2mop:slot-definition-name slot)
          :do (setf (gethash (string slot-name) table)
                    (slot-value x slot-name)))
    table))

(define-interface |Position| ()
  (line integer)
  (character integer))

(define-interface |Range| ()
  (start Position)
  (end Position))

(define-interface |Location| ()
  (uri string)
  (range Range))

(define-interface |Diagnostic| ()
  (range Range)
  (severity? integer)
  (code? (or integer string))
  (source? string)
  (message string))

(define-interface |Command| ()
  (title string)
  (command string)
  (arguments? list))

(define-interface |TextEdit| ()
  (range Range)
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

(define-interface |VersionedTextDocumentIdentifier| (TextDocumentIdentifier)
  (version integer))

(define-interface |TextDocumentPositionParams| ()
  (textDocument TextDocumentIdentifier)
  (position Position))

(define-interface |DocumentFilter| ()
  (language? string)
  (scheme? string)
  (pattern? string))

(define-interface |InitializeParams| ()
  (processId (or integer null))
  (rootPath (or string null))
  (rootUri (or string null))
  (initializationOptions? t)
  (capabilities ClientCapabilities)
  (trace? (member :off :messages :verbose)))
