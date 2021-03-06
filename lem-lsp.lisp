(defpackage #:lem-lsp
  (:use #:cl #:lem))
(in-package #:lem-lsp)

(defvar *workspaces* nil)
(defvar *client*)

(defstruct workspace
  language-id
  file-versions
  root
  client
  server-capabilities)

(defun client-language-id (client)
  (getf (jsonrpc:transport-data client) :language-id))

(defun start-lsp ()
  (setf *client*
        (jsonrpc:client-connect :host "127.0.0.1" :port 4389))
  (setf (jsonrpc:transport-data *client*)
        (list :language-id "go"))
  (add-hook *find-file-hook* 'text-document-did-open)
  (add-hook *after-save-hook* 'text-document-did-save)
  (add-hook *kill-buffer-hook* 'text-document-did-close)
  t)

(defun find-workspace (buffer)
  (dolist (workspace *workspaces*)
    (let ((result (mismatch (buffer-filename buffer)
                            (workspace-root workspace))))
      (when (and result (plusp result))
        (return workspace)))))

(defun buffer-workspace (&optional (buffer (current-buffer)))
  (get-bvar 'workspace :buffer buffer))

(defun (setf buffer-workspace) (workspace &optional (buffer (current-buffer)))
  (setf (get-bvar 'workspace :buffer buffer) workspace))

(defun get-root (client buffer)
  (declare (ignore client))
  (namestring (uiop:pathname-directory-pathname
               (buffer-filename
                buffer))))

(defun params (&rest plist)
  (alexandria:plist-hash-table plist :test 'equal))

(defun make-request (method params) (list method params))
(defun request-method (request) (first request))
(defun request-params (request) (second request))

(defvar *id-counter* 0)
(defun send-request (client request)
  #+(or)
  (lem::pdebug (list :request
                     (request-method request)
                     (with-output-to-string (out)
                       (yason:encode (request-params request) out))))
  (let ((transport client)
        (method (request-method request))
        (params (request-params request))
        (id (incf *id-counter*)))
    (jsonrpc:send-message (jsonrpc:make-request :id id
                                                :method method
                                                :params params)
                          transport)
    (loop
      (unless (usocket:wait-for-input (jsonrpc/transport/interface:transport-connection transport) :timeout 1)
        (return nil))
      (let ((response (jsonrpc:receive-message transport)))
        (when (jsonrpc/request-response::response-p response)
          (return (jsonrpc:response-result response)))))))

(defun send-notification (client request)
  #+(or)
  (lem::pdebug (list :notify
                     (request-method request)
                     (with-output-to-string (out)
                       (yason:encode (request-params request) out))))
  (jsonrpc:notify client
                  (request-method request)
                  (request-params request)))

(defun initialize (client buffer)
  (let* ((root (get-root client buffer))
         (workspace (make-workspace :language-id (client-language-id client)
                                    :file-versions (make-hash-table :test 'equal)
                                    :root root
                                    :client client)))
    (push workspace *workspaces*)
    (let ((response
           (send-request *client*
                         (make-request "initialize"
                                       (params "processId" (sb-posix:getpid)
                                               "rootPath" root
                                               "capabilities" (params))))))
      (setf (workspace-server-capabilities workspace)
            (gethash "capabilities" response)))))

(defun file-version (buffer workspace)
  (let ((file-versions (workspace-file-versions workspace)))
    (gethash (buffer-filename buffer) file-versions)))

(defun make-text-document-item (buffer workspace)
  (params "uri" (format nil "file://~A" (buffer-filename buffer))
          "languageId" (workspace-language-id workspace)
          "version" (file-version buffer workspace)
          "text" (points-to-string (buffers-start buffer) (buffers-end buffer))))

(defun lsp-buffer-p (buffer)
  (eq (buffer-major-mode buffer) 'lem.go-mode::go-mode))

(defun text-document-did-open (buffer)
  (when (lsp-buffer-p buffer)
    (let ((workspace (find-workspace buffer)))
      (unless workspace
        (assert (not (null *client*)))
        (initialize *client* buffer)
        (setf workspace (find-workspace buffer))
        (assert (workspace-p workspace)))
      (setf (buffer-workspace buffer) workspace)
      (setf (gethash (buffer-filename buffer) (workspace-file-versions workspace)) 0)
      (send-notification *client*
                         (make-request "textDocument/didOpen"
                                       (params "textDocument"
                                               (make-text-document-item buffer workspace)))))))

(defun text-document-did-save (buffer)
  (when (buffer-workspace buffer)
    (send-notification *client*
                       (make-request "textDocument/didSave"
                                     (params "textDocument"
                                             (make-text-document-identifier buffer))))))

(defun text-document-did-close (buffer)
  (let ((workspace (buffer-workspace buffer)))
    (when workspace
      (let ((file-versions (workspace-file-versions workspace)))
        (remhash (buffer-filename buffer) file-versions)
        (when (zerop (hash-table-count file-versions))
          (send-notification *client* (make-request "shutdown" (params))))
        (send-notification *client*
                           (make-request "textDocument/didClose"
                                         (params "textDocument" (make-version-text-document-identifier
                                                                 buffer
                                                                 workspace))))))))

(defun make-version-text-document-identifier (buffer workspace)
  (params "uri" (format nil "file://~A" (buffer-filename buffer))
          "version" (file-version buffer workspace)))

(defun make-text-document-identifier (buffer)
  (params "uri" (format nil "file://~A" (buffer-filename buffer))))

(defun make-position (point)
  (params "line" (1- (line-number-at-point point))
          "character" (point-charpos point)))

(defun make-text-document-position-params (point)
  (params "textDocument" (make-text-document-identifier (point-buffer point))
          "position" (make-position point)))

(define-key *global-keymap* "M-." 'lsp-find-definitions)
(define-command lsp-find-definitions () ()
  (let ((workspace (buffer-workspace (current-buffer))))
    (when workspace
      (let ((defs (send-request *client*
                                (make-request "textDocument/definition"
                                              (make-text-document-position-params (current-point)))))
            (elements '()))
        (message "~A" defs)
        (dolist (def defs)
          (let* ((uri (gethash "uri" def))
                 (range (gethash "range" def))
                 (start (gethash "start" range))
                 (line-number (gethash "line" start))
                 (character (gethash "character" start)))
            (ppcre:register-groups-bind (filename)
                ("^file://(.*)" uri)
              (let ((filename filename)
                    (line-number line-number)
                    (character character))
                (push (list def
                            (lambda ()
                              (find-file filename)
                              (line-offset (buffer-start (current-point))
                                           line-number
                                           character))
                            uri
                            line-number
                            character)
                      elements)))))
        (cond
          ((= 1 (length elements))
           (funcall (second (first elements))))
          ((not (null elements))
           (lem.sourcelist:with-sourcelist (sourcelist "*lsp-Definitions*")
             (loop :for (def jump-fn uri line-number character) :in elements
                   :do (lem.sourcelist:append-sourcelist
                        sourcelist
                        (lambda (point)
                          (insert-string point uri :attribute lem.grep::*attribute-1*)
                          (insert-string point ":")
                          (insert-string point (princ-to-string line-number) :attribute lem.grep::*attribute-2*)
                          (insert-string point ":")
                          (insert-string point (princ-to-string character) :attribute lem.grep::*attribute-2*))
                        jump-fn)))))))))
