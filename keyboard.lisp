(in-package :language)

(defparameter *russian-characters*
  '("фывапролджэ"
    "йцукенгшщзхъ"
    "ячсмитьбю"))

(defun quiz-keys (&key (line 0) (shuffle #'shuffle))
  (let ((line
          (etypecase line
            (number line)
            (cons (with-output-to-string (stream)
                    (iter (for index in line)
                          (write-string (nth index *russian-characters*) stream)))))))
    (let (remaining)
      (iter
       (when (null remaining)
         (setf remaining (funcall (or shuffle #'identity) (coerce line 'list))))
       (for char = (pop remaining))
       (format t "~A~%" char)
       (let ((in (read-char-from-emacs)))
         (if (char= in #\q)
           (return)
           (unless (char= in char)
             (format t "WRONG! (~A)~%" in))))))))
