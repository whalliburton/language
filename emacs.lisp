(in-package :language)

(defun eval-in-emacs (form)
  (funcall (find-symbol "EVAL-IN-EMACS" :swank) form))

(defun read-char-from-emacs ()
  (code-char (eval-in-emacs `(read-char))))

