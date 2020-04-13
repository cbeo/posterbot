;;;; posterbot.asd

(asdf:defsystem #:posterbot
  :description "A Matrix bot to post media links."
  :author "Colin Okay <cbeok@protonmail.com>"
  :license  "AGPL"
  :version "0.0.1"
  :serial t
  :depends-on (#:granolin #:cl-ppcre #:alexandria #:cl-fad #:drakma)
  :components ((:file "package")
               (:file "posterbot")))
