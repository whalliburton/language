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
        (when (string-equal word (car el))
          (collect el))))

(defparameter *frequency-wordlist-audio-mapping* nil)

(defun frequency-wordlist-audio-mapping (&optional reload)
  (if (or reload (null *frequency-wordlist-audio-mapping*))
    (setf *frequency-wordlist-audio-mapping*
          (let ((wordlist (list-shtooka-packet-words
                           '("rus-balm-voc" "rus-balm-voc-sakhno" "rus-nonfree")))
                (mapping (make-hash-table :test 'equal)))
            (iter (for word in (load-word-frequency-list))
                  (push (append (cdr word) (find-word-in-wordlist (car word) wordlist))
                        (gethash (car word) mapping)))
            mapping))
    *frequency-wordlist-audio-mapping*))

(defun print-frequency-wordlist-audio-mapping ()
  (iter (for el in (frequency-wordlist-audio-mapping))
    (format t "~A ~20T ~7A~{ ~A~}~%" (first el) (second el) (mapcar #'car (cddr el)))))

(defun list-frequency-words (&optional maximum-word-length strict-word-length)
  (iter
   (with count = 0)
   (for (name type) in (load-word-frequency-list))
   (when (or (null maximum-word-length)
             (and (null strict-word-length) (<= (length name) maximum-word-length))
             (= (length name) maximum-word-length))
     (collect name into words)
     (incf count))
   (finally (return (values words count)))))

(defun show-frequency-word-lengths ()
  (let ((acc (make-array 100 :initial-element 0)))
    (iter (for (name type) in (load-word-frequency-list))
          (incf (aref acc (length name))))
    (print-table
     (iter (for i from 0 to 99)
           (when (plusp (aref acc i))
             (collect (list i (aref acc i))))))))
