;;;; ex-lexicon.asd

(asdf:defsystem #:ex-lexicon
  :description "Describe ex-lexicon here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
               (:file "dict")
               (:file "ex-lexicon")))
