(defsystem :language
  :serial t
  :components ((:static-file "language.asd")
               (:file "packages")
               (:file "emacs")
               (:file "utilities")
               (:file "cached-http-request")
               (:file "iso-639")
               (:file "shtooka")
               (:file "frequency")
               (:file "vocab")
               (:file "keyboard"))
  :depends-on (:drakma :cl-json :iterate :usocket :babel :anaphora))



