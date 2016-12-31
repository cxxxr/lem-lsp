(cl:defpackage #:ts2cl
  (:use #:cl))
(in-package #:ts2cl)

(defun ts2cl (file)
  (let ((text (uiop:read-file-string file)))
    (parse text)))

(defun parse (text)
  (let ((pos 0)
        (str)
        (tokens '()))
    (loop
      (setf (values str pos) (scan text pos))
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

(defun scan (text &optional (start 0))
  (multiple-value-bind (start end start-groups end-groups)
      (ppcre:scan "^\\s*([a-zA-Z0-9_?]+|/\\*|.)" text :start start)
    (when start
      (let ((str (subseq text (aref start-groups 0) (aref end-groups 0))))
        (values str end)))))
