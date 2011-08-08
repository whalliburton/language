(in-package :language)

(defun cache-path ()
  (concatenate 'string (base-path) "cache/"))

(defun cache-filename-from-url (url)
  (concatenate 'string (cache-path) (substitute #\| #\/ url)))

(defun cached-http-request (url &key binary)
  (ensure-directories-exist (cache-path))
  (let ((filename (cache-filename-from-url url)))
    (if (probe-file filename)
      (with-input-from-file (stream filename :element-type (if binary '(unsigned-byte 8) 'character))
        (let ((seq (make-array (file-length stream)
                               :element-type (if binary '(unsigned-byte 8) 'character))))
          (read-sequence seq stream)
          seq))
      (let ((data (http-request url)))
        (with-output-to-file (stream filename :element-type (if binary '(unsigned-byte 8) 'character))
          (write-sequence data stream))
        data))))


