(in-package :language)

(defparameter *vocab-directory* (concatenate 'string (base-path) "vocab/"))

(defparameter *all-words*
  (list-shtooka-packet-words '("rus-balm-voc" "rus-balm-voc-sakhno" "rus-nonfree")))

(defvar *vocab* nil)

(defun load-vocab (&optional (directory *vocab-directory*))
  (let ((filename (choose-file directory (lambda (el) (substitute #\space #\- el)))))
    (setf *vocab*
          (with-input-from-file (stream filename)
            (iter (for line = (read-line stream nil))
              (while (and line (plusp (length line))))
              (destructuring-bind (native translation) (split-sequence #\| line)
                (collect (cons (string-trim '(#\space) native)
                               (string-trim '(#\space) translation)))))))
    (load-vocab-audio)
    (print-vocab)))

(defun print-vocab (&optional (vocab *vocab-audio*))
  (iter (for (native translated . audio) in vocab)
    (format t "~20A ~A~%" native translated)))

(defvar *vocab-audio* nil)

(defun find-audio-in-all-words (word)
  (iter
    (for (text . file) in *all-words*)
    (when (string= word text)
      (destructuring-bind (packet-name filename) (split-sequence #\/ file)
        (collect (shtooka-packet-word packet-name filename))))))

(defun load-vocab-audio (&optional (vocab *vocab*))
  (setf *vocab-audio*
        (iter (for (native . translation) in vocab)
          (let ((audio (find-audio-in-all-words native)))
            (collect
                (nconc
                 (list native
                       translation)
                 audio))))))

(defun play-vocab (&optional (vocab-audio *vocab-audio*))
  (iter (for (native translation . audio) in vocab-audio)
    (format t "~A~%" native)
    (mapcar #'play audio)))

(defun quiz-vocab (&key (vocab-audio *vocab-audio*) (shuffle t))
  (let (working removed)
    (iter
      (when (null working)
        (setf working (set-difference (if shuffle
                                        (shuffle (copy-list vocab-audio))
                                        vocab-audio) removed))
        (format t "** reshuffling~%~%"))
      (let ((current (pop working)))
        (destructuring-bind (native translation . audio) current
          (format t "~A" translation)
          (force-output)
          (let ((input (read-line)))
            (cond
              ((string= input "d") (push current removed))
              ((string= input "q") (return))))
          (format t "~A~%~%" native)
          (when audio (play (random-nth audio))))))))
