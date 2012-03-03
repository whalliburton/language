(in-package :language)

(defun define-word (word)
  (with-dict-client (client)
    (iter (for definition in (define client word))
          (let ((description (dict:definition definition)))
            (collect
                (list
                 (dict:database definition)
                 (first description)
                 (format nil "~{~A~%~}" (rest description))))))))

(defun show-definitions (word)
  (loop for (dictionary word definition) in  (define-word word)
        do (format t "~A~%~A~%~A~%" dictionary word definition)))
