;;;; ex-lexicon.asd

(asdf:defsystem #:ex-lexicon
  :description "Ex-Lexicon: A conlanger dictionary tool."
  :author "Peter Keller <psilord@cs.wisc.edu>"
  :license  "MIT License"
  :version "0.0.1"
  :serial t
  :depends-on (#:alexandria)
  :components ((:file "package")
	       (:file "utils")
               (:file "ex-lexicon")
	       ;; TODO: Put language definitions here.
	       ;; Actual dictionaries must come last.
	       (:file "dicts/kilta")
	       ))
