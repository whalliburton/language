(in-package :language)

(defparameter *vocab-directory* (concatenate 'string (base-path) "vocab/"))
(defparameter *fonts-directory* (concatenate 'string (base-path) "fonts/"))
(defparameter *flashcards-directory* (concatenate 'string (base-path) "flashcards/"))

(defparameter *all-words*
  (list-shtooka-packet-words '("rus-balm-voc" "rus-balm-voc-sakhno" "rus-nonfree")))

(defvar *vocab* nil)
(defvar *vocab-filename* nil)

(defun load-vocab (&optional (directory *vocab-directory*))
  (let ((filename (choose-file directory (lambda (el) (substitute #\space #\- el)))))
    (setf *vocab*
          (with-input-from-file (stream filename)
            (iter (for line = (read-line stream nil))
              (while line)
              (when (and (plusp (length line)) (not (char= (char line 0) #\#)))
                (destructuring-bind (native translation) (split-sequence #\| line)
                  (collect (cons (string-trim '(#\space) native)
                                 (string-trim '(#\space) translation)))))))
          *vocab-filename* filename)
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

(defconstant +points-per-inch+ 72)

(defparameter *vocab-fonts*
  `(("rosa-arion-normal" "ROSAArionWC")
    ("liberation-sans-regular" "LiberationSans")))

(defvar *front-vocab-font* nil)
(defvar *back-vocab-font* nil)

(defun load-vocab-fonts ()
  (iter (for (file name) in *vocab-fonts*)
        (pdf:load-ttu-font (format nil "~A~A.ufm" *fonts-directory* file)
                           (format nil "~A~A.ttf" *fonts-directory* file))
        (collect (pdf:get-font name))
        (if (first-iteration-p)
          (setf *front-vocab-font* name)
          (setf *back-vocab-font* name))))

(defun generate-vocab-flashcards (&key (number-per-card 5)
                                       (cards-per-row 3)
                                       (cards-per-column 3)
                                       (row-padding 20)
                                       (column-padding 20))
  (assert *vocab-filename*)
  (ensure-directories-exist *flashcards-directory*)
  (load-vocab-fonts)
  (let* ((set-name (pathname-name *vocab-filename*))
         (filename (format nil "~A~A-flashcards.pdf" *flashcards-directory* set-name))
         (width (* 2.4d0 +points-per-inch+))
         (height (* 3.3d0 +points-per-inch+))
         (cl-pdf:*default-page-bounds* cl-pdf:*letter-portrait-page-bounds*))
    (labels ((draw-card (dx dy words back)
               (let ((center-block
                       (compile-text ()
                         (with-style (:font *back-vocab-font* :font-size 6)
                           (unless back (typeset::put-string set-name)))
                         (paragraph
                             (:h-align :center
                              :font (if back *back-vocab-font* *front-vocab-font*)
                              :font-size 18)
                           :vfill
                           (iter
                            (for word on words)
                            (typeset::put-string (if back (cdar word) (caar word)))
                            (when (cdr words) (typeset:vspace 20)))
                           :vfill))))
                 (pdf:translate dx dy)
                 (typeset::draw-block center-block 0 height width height :v-align :fill :border 1)
                 (pdf:translate (- dx) (- dy))))
             (draw-page (rows &optional back)
               (pdf:with-page ()
                 (iter (for row in rows)
                       (for ncol from 0)
                       (iter (for card in (if back (reverse row) row))
                             (for nrow from 0)
                             (let ((dx (+ 5 (+ row-padding (* (+ width row-padding) nrow))))
                                   (dy (+ column-padding (* (+ height column-padding) ncol))))
                               (draw-card dx dy card back)))))))
      (pdf:with-document ()
        (let* ((cards (group *vocab* number-per-card))
               (rows (group cards cards-per-row))
               (pages (group rows cards-per-column)))
          (iter (for page in pages)
                (draw-page page)
                (draw-page page t)))
        (pdf:write-document filename)))))