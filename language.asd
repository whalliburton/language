(defsystem :language
  :serial t
  :components ((:static-file "language.asd")
               (:file "packages")
               (:file "pretty-printing")
               (:file "emacs")
               (:file "utilities")
               (:file "cached-http-request")
               (:file "iso-639")
               (:file "shtooka")
               (:file "frequency")
               (:file "vocab")
               (:file "keyboard")
               (:file "help")
               (:file "translate")
               (:file "festival")
               (:file "dictionary")
               (:file "quiz")
               (:file "initialize"))
  :depends-on (:drakma :cl-json :iterate :usocket :babel :anaphora :cl-fad
                       :flexi-streams :org-davep-dict
                       :cl-typesetting))
