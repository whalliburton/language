;; festival.lisp

(in-package :language)

(defparameter *festival-key* (string-to-octets "ft_StUfF_key"))
(defvar *festival-socket* nil)
(defvar *festival-stream* nil)
(defvar *festival-server-process* nil)

(defun connect-to-festival (&key (host "localhost") (port 1314))
  (setf *festival-socket* (usocket:socket-connect host port :element-type '(unsigned-byte 8))
        *festival-stream* (usocket:socket-stream *festival-socket*)))

(defun ensure-festival-server-started ()
  (when (null (probe-file "/usr/bin/festival"))
    (format t "Festival is not installed.~%")
    (return-from ensure-festival-server-started nil))
  (unless (aand *festival-server-process* (eq (process-status it) :running))
    (setf *festival-server-process*
          (run-program "/usr/bin/festival" '("--server") :wait nil))
    (sleep 1) ;; TODO should properly wait for it to start
    (connect-to-festival))
  t)

(defun speak-in-russian ()
  "Set the speaking language to Russian."
  (values (send-to-festival "(voice_msu_ru_nsh_clunits)")))

(defun speak-in-english ()
  "Set the speaking language to English."
  (values (send-to-festival "(voice_kal_diphone)")))

(defun say (phrase)
  "Speak PHRASE."
  (send-to-festival (format nil "(SayText ~s)" phrase))
  phrase)

;; Audio_Method Audio_Command
(defun festival-parameter (name)
  (send-to-festival (format nil "(Parameter.get '~a)" name)))

(defun set-festival-parameter (name val)
  (send-to-festival (format nil "(Parameter.set '~a '~a)" name val)))

(defun read-binary-line (stream)
  (octets-to-string
   (with-output-to-sequence (str)
     (loop for char = (read-byte stream)
           while (/= char #.(char-code #\Newline))
           do (write-byte char str)))
   :external-format :utf-8))

(defun read-waveform (stream)
  (with-output-to-sequence (str)
    (loop for char = (read-byte stream)
          while (and (listen stream)
                     ;; HACK
                     (not (string-ends-with
                           (flexi-streams::vector-stream-vector str)
                           *festival-key* :test #'=)))
          do (write-byte char str))))

(defun send-to-festival (str)
  (when (ensure-festival-server-started)
    (write-sequence (string-to-octets (format nil "~a~%" str) :external-format :utf-8)
                    *festival-stream*)
    (force-output *festival-stream*)
    (let ((rtn (read-binary-line *festival-stream*)))
      (values
        (cond
          ((string= rtn "LP") (read-binary-line *festival-stream*))
          ((string= rtn "ER") (error "festival error"))
          ((string= rtn "WV") (read-waveform *festival-stream*))
          (t (error "unknown festival return ~a" rtn)))
        (loop while (listen *festival-stream*)
              collect (read-binary-line *festival-stream*))))))

(defun process-festival-arg (arg)
  (with-output-to-string (str)
    (etypecase arg
      (keyword (format str "~a" (string-downcase arg)))
      (symbol (format str "'~a" (string-downcase arg)))
      (string (print arg str)))))

(defmacro define-festival-command (name &rest args)
  (let ((funname (if (consp name) (first name)
                   (substitute-if #\- (lambda (el) (member el '(#\. #\_)))
                                  (string-upcase name))))
        (fesname (if (consp name) (second name) name)))
    `(defun ,(symb 'festival- funname) (,@args)
       (let ((raw (send-to-festival
                   (format nil "(~a~{ ~a~})" ,fesname
                           ,(when args `(mapcar #'process-festival-arg
                                                (list ,@args)))))))
         (if (or (vectorp raw) (string-starts-with raw "#<"))
           raw
           (safe-read-from-string raw))))))

(define-festival-command "voice.list")
(define-festival-command "voice.description" name)
(define-festival-command (lex-lookup "lex.lookup_all") word)
(define-festival-command "Utterance" type data)
(define-festival-command "tts_textall" text mode)

(defmacro define-festival (name)
  `(defun ,(symb 'festival- (substitute #\- #\. (string-upcase name))) ()
     (safe-read-from-string (send-to-festival ,name))))

(define-festival "current-voice")
