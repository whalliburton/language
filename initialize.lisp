(in-package :language)

(defun initialize ()
  (load-translation-cache))

(defun done ()
  "Save current progress."
  (save-translation-cache))
