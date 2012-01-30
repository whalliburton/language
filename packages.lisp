;; packages.lisp

(defpackage language
  (:use common-lisp drakma anaphora split-sequence iterate)
  (:import-from alexandria when-let with-input-from-file with-output-to-file shuffle)
  (:import-from cl-fad list-directory)
  (:import-from sb-ext run-program)
  (:export quiz-keys))

