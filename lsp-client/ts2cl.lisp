(cl:defpackage #:ts2cl
  (:use #:cl))
(in-package #:ts2cl)

(defvar *tokens*)

(defun ts2cl (file)
  (let ((text (uiop:read-file-string file)))
    (parse text)))

(defun parse (text)
  (let ((*tokens* (scan-text text)))
    (catch 'back
      (=interface))))

(defun lookahead ()
  (first *tokens*))

(defun next ()
  (pop *tokens*)
  t)

(defun maybe (name)
  (when (equal name (lookahead))
    (next)
    t))

(defun match (name)
  (if (equal name (lookahead))
      (next)
      (throw 'back t)))

(defun =interface ()
  (maybe "export")
  (match "interface")
  (let ((name (=name)))
    (list :interface name (=key-value-list))))

(defun =key-value-list ()
  (and (maybe "{")
       (loop :until (maybe "}")
             :collect (=key-value))))

(defun =key-value ()
  (let (name optionalp value)
    (and (setf name (=name))
         (progn (setf optionalp (maybe "?")) t)
         (progn
           (match ":")
           (setf value (=value))
           (match ";")
           (list name value optionalp)))))

(defun =value ()
  (or (=name-or-array)
      (=key-value-list)))

(defun =name ()
  (let ((str (lookahead)))
    (and (stringp str)
         (loop :for c :across str
               :do (unless (alphanumericp c)
                     (return nil))
               :finally (return t))
         (progn
           (next)
           str))))

(defun =name-or-array ()
  (let ((name (=name)))
    (when name
      (cond
        ((maybe "[")
         (match "]")
         (list :array name))
        (t
         name)))))

(defun scan-text (text)
  (let ((pos 0)
        (str)
        (tokens '()))
    (loop
      (setf (values str pos) (scan-ahead text pos))
      (unless str (return))
      (cond ((string= str "/*")
             (setf pos (search "*/" text :start2 (+ pos 2)))
             (unless pos
               (warn "comment end does not found.")
               (return))
             (incf pos 2))
            ((string= str "'")
             (let ((quote-start (1- pos)))
               (setf pos (search "'" text :start2 (+ pos 1)))
               (unless pos
                 (warn "string end does not found.")
                 (return))
               (incf pos 1)
               (push (subseq text quote-start pos) tokens)))
            (t
             (push str tokens))))
    (nreverse tokens)))

(defun scan-ahead (text &optional (start 0))
  (multiple-value-bind (start end start-groups end-groups)
      (ppcre:scan "^\\s*([a-zA-Z0-9_]+|\\?|/\\*|.)" text :start start)
    (when start
      (let ((str (subseq text (aref start-groups 0) (aref end-groups 0))))
        (values str end)))))
