(in-package #:ex-lexicon)

;; This huge thing is just to test adding large entries and a lot of them into
;; the dictionary. Used for scaling purposes.
;; Lifted from Kílta
(defmacro deflex-ntimes (ntimes)
  `(progn
     ,@(loop :for i :below ntimes
             :for name = (format nil "test-~A" i)
             :collect
             `(deflex ,name ()
                (sense (pos (e "v.tr"))
                       (gloss (e "adapt (to)"))
                       (definition (e "fit, adapt (to), accomodate"))
                       (example (v "kotora në ta onnít si hollo")
                                (e "the chair fits this room")))
                (sublem "X li hollo"
                        (sense (pos (e "idiom"))
                               (definition (e "agree with"))
                               (example (v "ton li hollo më")
                                        (e "I don't agree with you"))
                               (example (v "ën mëmítot li hollo")
                                        (e "she agreed with this plan"))
                               (example (v "ha li ahët hollo núnë")
                                        (e "they agreed with me unwillingly"))))
                (sublem "holliso"
                        (sense (pos (e "v.in"))
                               (definition (e "fit (well)"))
                               (example (v "ta tëkímës holliso më")
                                        (e "that hat doesn't fit")))
                        (sense (pos (e "v.in"))
                               (definition (e "suit, be suitable for, be appropriate for"))
                               ;; v e are also strcat forms to allow intermixing or multiline:
                               ;; (e "hello " "world")
                               ;; (e "foo" (v "bar"))
                               (usage (e "with " (v "kë")))
                               (example (v "ën saka në ha vë para kë hollisat no re")
                                        (e "this idea will suit my book"))
                               (example (v "kochis hunta kë holliso më")
                                        (e "a squirrel is not a suitable sacrifice")))
                        (sense (pos (e "v.vin"))
                               (definition (e "properly, suitably, appropriately"))
                               (usage (e "with converb"))
                               (example (e "hil mítár holliso")
                                        (v "she spoke properly"))
                               (example (v "mélá mai tërë kalúnët holliso")
                                        (e "I visit my parents now, as is proper")))
                        (sense (pos (e "v.in"))
                               (definition (e "agree grammatically"))
                               (usage (e "with " (v "li ") "for target, "
                                         (v "kwan ") "for feature"))
                               (example
                                (v "\"tu\" ka në \"antëstá\" ka li olta kwan holliso")
                                (e (v "\"tu\"") agrees with (v "\"antësá\"") "in number"))))
                (sublem "hollët naho"
                        (sense (pos (e "v.tr"))
                               (definition (e "deal with, cope with"))
                               (example (v "ël si tërë së hollët nahat harno më")
                                        (e "I can't deal with her right now"))))))))


;; I tried this, and inserting it during compilation took 30 seconds, but it
;; worked. So, we'll test later when emitting the entire dictionary including
;; all transformations and presentation passes to see how long it takes.
;;
;; (deflex-ntimes 500000)


(deflex "junk" ()
  (morphology (e "slfkdlsdkjf"))
  (see (e "ijoijoij"))
  (sense
   (pos (e "n"))
   (definition (e "thing")))
  (senses
   (sense
    (pos (e "v.tr"))
    (definition (e "stuff") (v "fff"))
    (usage (e "poop"))
    (example (v "dlsjsdlfjsdlkjf")
             (e (strcat (e "lsdkjfsldkjflskdjf ") (v "li ") (e "stuff"))))
    (example (v "lskdjfsdlkfj")
             (e "sldkjldksfj"))))
  (sense
   (pos (e "n"))
   (definition (e "other thing"))
   (see (e "lasnsljn")))
  (sublem "froggy"
          (sense
           (pos (e "adj"))
           (gloss (e "lsjkdf"))
           (definition (e "sljndc"))))
  (sublem "pkpok"
          (sense
           (pos (e "interj"))
           (gloss (e "lsjkdf"))
           (definition (e "sljndc"))))
  (sublems
   (sublem "sldhfjsdlfj"
           (sense
            (pos (e "pro"))
            (definition (e "lsdnlsdnclsdknc"))))))


(deflex "junk2" ()

  (wumpscut "hello" "world")
  (froboz "thingy" (e "stuff"))

  (pronounciation (ipa "shirt"))
  (etymology (e "shoes"))
  (senses
   (sense
    (pos (e "ppp")
         (gloss (e "ooo")))))
  (sense (pos (e "xxx")) (gloss (e "bbb")))
  (thingy "xfff")
  (sublem "junk2ish"
          (sense
           (label :xxx)
           (pos (e "n"))
           (gloss (e "foo"))
           (definition (e "foo, no really")))))

(deflex "junk3" ()
  (homonym
   (pronounciation (ipa "shirt"))
   (etymology (e "shoes"))
   (senses
    (sense
     (pos (e "ppp")
          (gloss (e "ooo")))))
   (sense (pos (e "xxx")) (gloss (e "bbb")))
   (thingy "xfff")
   (sublem "junk2ish"
           (sense
            (label :xxx)
            (pos (e "n"))
            (gloss (e "foo"))
            (definition (e "foo, no really")))))

  (homonym
   (etymology (e "hats"))
   (sense
    (pos (e "LLL")
         (definition (e "MMM"))))))

;; Testing if I can lift the full canonical form out of this mess.
(deflex "bare" ()
  (pos (e "v.tr"))
  (gloss (e "stuff"))
  (definition (e "a longer verion of stuff"))
  (example (v "I bare the toad.")
           (e "really, I did it.")))
