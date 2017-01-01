(cl:defpackage #:ts2cl
  (:use #:cl))
(in-package #:ts2cl)

(defvar *tokens*)

(defun ts2cl (file)
  (let ((text (uiop:read-file-string file)))
    (parse text)))

(defun parse (text)
  (let ((*tokens* (scan-text text)))
    (catch 'fail
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
      (throw 'fail (list *tokens*
                         (with-output-to-string (stream)
                           (uiop:print-backtrace :stream stream))))))

(defun =interface ()
  (maybe "export")
  (match "interface")
  (let ((name (=name))
        (extends))
    (when (maybe "extends")
      (setf extends (=interface-extends)))
    (let ((definitions (=var-type-list)))
      `(define-interface ,(alexandria:symbolicate name) (,@extends)
         ,@(mapcar (lambda (definition)
                     (destructuring-bind (var type)
                         definition
                       `(,var ,type)))
                   definitions)))))

(defun =interface-extends ()
  (let ((extends '()))
    (loop
      (let ((name (=name)))
        (push name extends)
        (unless (maybe ",")
          (return))))
    (mapcar #'alexandria:symbolicate (nreverse extends))))

(defun =var-type-list ()
  (and (maybe "{")
       (loop :until (maybe "}")
             :collect (=var-type))))

(defun =var-type ()
  (let ((var (=name)))
    (when var
      (let ((optionalp (maybe "?")))
        (match ":")
        (let ((type (=typespec)))
          (let ((rest-types
                 (loop :while (maybe "|")
                       :collect (=typespec))))
            (when rest-types
              (setf type `(or ,type ,@rest-types)))
            (maybe ";")
            (list (if optionalp
                      (alexandria:symbolicate (string-upcase var) "?")
                      (alexandria:symbolicate (string-upcase var)))
                  type)))))))

(defun =typespec ()
  (let ((type (=type)))
    (if (null type)
        `(json ,@(=var-type-list))
        type)))

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

(defun =type ()
  (let ((name (=name)))
    (when name
      (cond
        ((maybe "[")
         (match "]")
         `(trivial-types:proper-list
           ,(ts-to-lisp-type name)))
        (t
         (ts-to-lisp-type name))))))

(defun ts-to-lisp-type (name)
  (cond
    ((equal name "boolean") 'boolean)
    ((equal name "number") 'integer)
    ((equal name "string") 'string)
    ((equal name "any") 'T)
    (t (intern name))))

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
