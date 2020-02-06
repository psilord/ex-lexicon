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
               ;; TODO: Later, this will be a specific load pass so
               ;; then the dictionary gets big I don't have to recompile it
               ;; all the time and instead just load and dump it when
               ;; desired.
               (:file "dicts/test")
               (:file "dicts/kilta")
               ))
