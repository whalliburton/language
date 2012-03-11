;; translate.lisp

(in-package :language)

(defparameter *google-api-key*
  (let ((filename (format nil "~Agoogle-api-key.txt" (base-path))))
    (if (not (probe-file filename))
      (warn "No google key found at ~S." filename)
      (string-trim '(#\newline) (slurp-file filename)))))

(defun local-url-encode (string)
  (with-output-to-string (stream)
    (iter (for char in-vector string)
          (if (alphanumericp char)
            (write-char char stream)
            (format stream "%~2,'0x" (char-code char))))))

(defun google-translate (query source target &key (key *google-api-key*))
  (let ((*drakma-default-external-format* :utf-8))
    (multiple-value-bind (data code)
        (http-request
         (format nil "https://www.googleapis.com/language/translate/v2~
                    ?key=~A&source=~A&target=~A&q=~A" key source target (local-url-encode query)))
      (when (eql code 200)
        (decode-json-from-string (sb-ext:octets-to-string data :external-format :utf-8))))))

(defvar *translation-cache* (make-hash-table :test 'equal))

(defun translation-cache-filename ()
  (format nil "~Atranslation-cache"  (base-path)))

(defun save-translation-cache ()
  (with-output-to-file (stream (translation-cache-filename) :if-exists :supersede)
    (prin1 (iter (for (k v) in-hashtable *translation-cache*)
                 (collect (list k v))) stream))
  t)

(defun load-translation-cache ()
  (iter (for (k v) in (with-input-from-file (stream (translation-cache-filename))
                        (read stream)))
        (setf (gethash k *translation-cache*) v)))

(defun translate (query &key (source "ru") (target "en"))
  "Translate QUERY from one language to another."
  (or (gethash (list query source target) *translation-cache*)
      (setf (gethash (list query source target) *translation-cache*)
            (let ((json (google-translate query source target)))
              (when json
                (mapcar (lambda (el)
                          (cdr (assoc :translated-text el)))
                        (cdr (assoc :translations
                                    (cdr (assoc :data json))))))))))
