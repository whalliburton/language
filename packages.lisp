;; packages.lisp

(defpackage language
  (:use common-lisp drakma anaphora split-sequence iterate typeset)
  (:import-from alexandria when-let with-input-from-file with-output-to-file shuffle)
  (:import-from cl-fad list-directory)
  (:import-from json decode-json-from-string)
  (:import-from sb-ext run-program process-status octets-to-string string-to-octets)
  (:import-from flexi-streams with-output-to-sequence)
  (:import-from dict with-dict-client define)
  (:export quiz-keys quiz-typing translate quiz done))

