(in-package :language)

(defparameter *russian-characters*
  '("фывапролджэ"
    "йцукенгшщзхъ"
    "ячсмитьбю"))

(defun quiz-keys (&key (line '(0 1 2)) (shuffle #'shuffle))
  "Simple quiz of the Russian keyboard."
  (format t "Press 'q' to exit.~%")
  (let ((line
          (etypecase line
            (number (nth line *russian-characters*))
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

(defun quiz-typing (&optional maximum-word-length)
  "Simple quiz of typing Russian words."
  (format t "Type 'q' to exit.~%")
  (let* ((words
           (iter (for (name type) in (load-word-frequency-list))
                 (if (or (null maximum-word-length)
                         (<= (length name) maximum-word-length))
                  (collect name))))
         (length (length words)))
    (iter
     (let ((word (nth (random length) words)))
       (format t "~A~&" word)
       (let ((input (read-line)))
         (when (equal input "q") (return))
         (when (not (equal input word))
           (format t "WRONG!~%")))))))
