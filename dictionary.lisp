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
