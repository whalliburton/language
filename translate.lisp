;; translate.lisp

(in-package :language)

(defparameter *google-api-key*
  (let ((filename (format nil "~Agoogle-api-key.txt" (base-path))))
    (if (not (probe-file filename))
      (warn "No google key found at ~S." filename)
      (string-trim '(#\newline) (slurp-file filename)))))

(defun url-encode (string)
  (with-output-to-string (stream)
    (iter (for char in-vector string)
          (if (alphanumericp char)
            (write-char char stream)
            (format stream "%~2,'0x" (char-code char))))))

(defun google-translate (query source target &key (key *google-api-key*))
  (multiple-value-bind (data code)
      (http-request
       (format nil "https://www.googleapis.com/language/translate/v2~
                    ?key=~A&source=~A&target=~A&q=~A" key source target (url-encode query))
       :external-format-out :utf-8 )
    (when (eql code 200)
      (decode-json-from-string (sb-ext:octets-to-string data)))))

(defun translate (query &key (source "en") (target "ru"))
  "Translate QUERY from one language to another."
  (let ((json (google-translate query source target)))
    (when json
      (mapcar (lambda (el)
                (cdr (assoc :translated-text el)))
              (cdr (assoc :translations
                          (cdr (assoc :data json))))))))
