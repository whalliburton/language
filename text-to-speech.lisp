(in-package :language)

(defun text-to-speech (text &key (language "ru"))
  (let ((url (format nil "http://translate.google.com/translate_tts?tl=~A&q=~A" language text)))
    (cached-http-request url :binary t)
    (play-audio-file (cache-filename-from-url url))))
