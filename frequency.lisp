;; frequency.lisp

(in-package :language)

(defun frequency-list-path ()
  (concatenate 'string (base-path) "russian/5000-frequency.txt"))

(defparameter *word-frequency-list* nil)

(defun load-word-frequency-list (&optional reload)
  (if (or reload (null *word-frequency-list*))
    (setf *word-frequency-list*
          (iter (for line in-file (frequency-list-path)
                     using #'read-line)
                (collect (cddr (split-sequence #\space (string-trim '(#\return) line))))))
    *word-frequency-list*))

(defun find-word-in-wordlist (word wordlist)
  (iter (for el in wordlist)
        (when (or (string-equal word (car el))
;;                  (string-starts-with (car el) word)
                  )
          (collect el))))

(defun match-frequency-to-wordlist ()
  (let ((wordlist (list-shtooka-packet-words
                   '("rus-balm-voc" "rus-balm-voc-sakhno" "rus-nonfree"))))
    (iter (for word in (load-word-frequency-list))
          (collect (append word (find-word-in-wordlist (car word) wordlist))))))

(defun print-frequency-wordlist-matches ()
  (iter (for el in (match-frequency-to-wordlist))
    (format t "~A ~20T ~7A~{ ~A~}~%" (first el) (second el) (mapcar #'car (cddr el)))))