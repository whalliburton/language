(in-package :language)

(defun parse-shtooka-tags (data)
  (with-input-from-string (stream data)
    (iter
      (for line = (read-line stream nil))
      (while line)
      (when (and (plusp (length line)) (char= #\[ (char line 0)))
        (collect
            (list (subseq line 1 (1- (length line)))
                  (iter
                    (for line = (read-line stream nil))
                    (while line)
                    (when (zerop (length line)) (return tags))
                    (when-let (pos (position #\= line))
                      (collect
                          (cons (intern (substitute #\- #\_ (subseq line 0 pos)) :keyword)
                                (subseq line (1+ pos) (length line)))
                        into tags)))))))))

(defun list-shtooka-packets ()
  (parse-shtooka-tags
   (cached-http-request "http://packs.shtooka.net/index.packs.txt")))

(defun list-shtooka-packet-names ()
  (mapcar #'first (list-shtooka-packets)))

(defun list-shtooka-languages ()
  (sort
   (mapcar 'language-name-from-code
           (remove-duplicates
            (mapcar (lambda (el)
                      (cond
                        ((string= el "cm") "zh")
                        ((string= el "po") "pt")
                        ((string= el "sp") "es")
                        ((string= el "wu") "zh")
                        (t el)))
                    (mapcar (lambda (el) (cdr (assoc :pack-langs (second el))))
                            (list-shtooka-packets))) :test #'string=))
   #'string<))

(defun ensure-valid-shtooka-packet-name (name)
  (unless (member name (list-shtooka-packet-names) :test #'string=)
    (error "No shtooka packet named ~S." name)))

(defun list-shtooka-packet-words (packet-names)
  (etypecase packet-names
    (cons (iter (for name in packet-names)
                (nconcing (list-shtooka-packet-words name))))
    (string
       (ensure-valid-shtooka-packet-name packet-names)
       (mapcar
        #L(cons (cdr (assoc :swac-text (second %)))
                (concatenate 'string packet-names "/" (first %)))
        (parse-shtooka-tags
         (cached-http-request
          (format nil "http://packs.shtooka.net/~a/ogg/index.tags.txt" packet-names)))))))

(defun ensure-valid-shtooka-packet-word (packet-name filename)
  (or (assoc filename (list-shtooka-packet-words packet-name) :test #'string=)
      (error "Shtooka packet ~s does not have a file named ~s." packet-name filename)))

(defun print-shtooka-packet-words (packet-name)
  (iter (for el in (list-shtooka-packet-words packet-name))
    (format t "~A ~30T~A~%" (car el) (cdr el))))

(defclass shtooka-word ()
  ((text :initarg :text :reader shtooka-word-text)
   (url :initarg :url :reader shtooka-word-url)))

(defmethod print-object ((shtooka-word shtooka-word) stream)
  (print-unreadable-object (shtooka-word stream :type t)
    (format stream "~A" (shtooka-word-text shtooka-word))))

(defun shtooka-text-from-filename (packet-name filename)
  (car
   (find (concatenate 'string packet-name "/" filename)
         (list-shtooka-packet-words packet-name) :key 'cdr :test 'string=)))

(defun shtooka-packet-word (packet-name filename)
  (ensure-valid-shtooka-packet-name packet-name)
  (let ((url (format nil "http://packs.shtooka.net/~a/ogg/~a" packet-name filename)))
    (cached-http-request url :binary t)
    (make-instance 'shtooka-word
                   :text (shtooka-text-from-filename packet-name filename)
                   :url url)))

(defmethod play ((word shtooka-word))
  (run-program "ogg123" (list (cache-filename-from-url (shtooka-word-url word)))
               :wait t :search t))
