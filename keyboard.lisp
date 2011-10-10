(in-package :language)

(defparameter *russian-characters*
  '("фывапролджэ"
    "йцукенгшщзхъ"
    "ЯЧСМИТЬБЮ"
    "ёйцукенгшщзхъфывапролджэячсмитьбю"))

(defun quiz-keys (&key (line 0) (shuffle #'shuffle))
  (let (remaining)
    (iter
     (when (null remaining)
       (setf remaining (funcall (or shuffle #'identity)
                                (coerce (nth line *russian-characters*) 'list))))
     (for char = (pop remaining))
     (format t "~A~%" char)
     (let* ((line (read-line))
            (in (aref line 0)))
       (if (char= in #\q)
         (return)
         (unless (char= in char)
           (format t "WRONG!~%")))))))


