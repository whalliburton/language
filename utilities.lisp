(in-package :language)

(defun base-path ()
  (directory-namestring (asdf:component-pathname (asdf:find-system :language))))

(defun string-starts-with (string prefix &key (test #'char=))
  "Returns true if STRING starts with PREFIX."
  (let ((mismatch (mismatch prefix string :test test)))
    (or (not mismatch) (= mismatch (length prefix)))))

(defun random-nth (list)
  (elt list (random (length list))))

(defun choose-file (directory &optional (name-processor #'identity))
  (let ((indexed
          (iter (for filename in (list-directory directory))
            (with index = 0)
            (when (pathname-name filename)
              (incf index)
              (format t "~3A ~A~%" index (funcall name-processor (pathname-name filename)))
              (collect (cons index filename))))))
    (cdr (assoc (parse-integer (read-line)) indexed :test #'=))))

(defun slurp-file (filename)
  "Load then entire file of FILENAME into a string."
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let ((seq (make-string (file-length stream))))
      (read-sequence seq stream)
      seq)))
