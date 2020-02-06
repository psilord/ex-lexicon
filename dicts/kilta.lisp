(in-package #:ex-lexicon)

;;;;
(deflex "á" ()
  (morphology (e "pl. -á"))
  (etymology (e "from \\bold{ahë}"))
  ;; Order of senses is always in order as denoted.
  (senses
   (sense
    (label :xxx) ;; label may be omitted, used for references.
    (pos (e "interj"))
    (gloss (e "yeah"))
    ;; first arg is actual definition, rest args are concatenated into
    ;; an explanation string
    (definition (e "yeah"))
    (usage (e "colloquial and informal for \\bold{ahë} yes"))))
  (sublems
   (sublem "á tul"
           (senses
            (sense
             (pos (e "idiom"))
             (gloss (e "right?"))
             (definition (e "right?"))
             (usage (e "isn't it?" "colloquial tag question"))
             (examples
              (example (v "símur tëníto, á tul?")
                       (e "They're gone, right?"))))))
   (sublem "áppi"
           (sees
            (see (ref lexeme "áhë" sublem "áhëppi"))))))

;;;;
(deflex "achëm" ()
  (senses
   (sense (pos (e "n"))
          (definition (e "used in the idiom:"))))
  (sublems
   (sublem "achëm li"
           (senses
            (sense
             (pos (e "idiom"))
             (definition (e "at first, initially"))
             (examples
              (example (v "áppi achëm li saro, luë mëppi tërë saro")
                       (e "I used to think so, but not I don't"))))))))

;;;;
(deflex "áchilëm" ()
  (senses
   (sense (pos (e "n"))
          (definition (e "(an episode of) vomiting"))))
  (sublems
   (sublem "áchilëm so míto"
           (senses
            (sense
             (pos (e "idiom"))
             (definition (e "hurl"))
             (usage (e "a slightly jocular, slightly off-color "
                       "expression for throwing up"))
             (examples
              (example (v "ël në vincho vë ëmar si ilët, áchilëm so míto")
                       (e "She drank some off milk and then hurled"))))))))

;;;;
(deflex "áchilo" ()
  (senses
   (sense
    (pos (e "v.tr, v.in"))
    (definition (e "vomit, throw up"))
    (examples
     (example (v "kwilë sanët, kovura si áchilo")
              (e "he ate too much and threw everything up"))
     (example (v "mauta në chërava nen huchë áchilo tul?")
              (e "Did the cat throw up on the carpet again?"))))))

;;;;
(deflex "acho" ()
  (sense
   (pos (e "v.tr"))
   (definition (e "go up, ascend"))
   (example (v "nama në achirë")
            (e "the sun in rising"))
   (example (v "këlleka si achi rum")
            (e "let's go up the hill"))
   (example (v "molán në nankië achirë")
            (e "the smoke is rising into the sky"))
   (example (v "ëlá në tavár vima mai acho")
            (e "they ran up to the town")))
  (sense
   (pos (e "v.tr"))
   (definition (e "climb"))
   (example (v "ammësá kiva si acho")
            (e "vines climb the tree"))))

;;;;
(deflex "achún" ()
  (senses
   (sense
    (pos (e "n"))
    (definition (e "grass"))
    (examples
     (example (v "ilivët, achún ralin chaisrë")
              (e "it rained and grass is getting green"))))))

;;;;

;; do naho, micha, latëmo, luëta, lur
;;;;



(deflex "hollo" ()
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
                          (e "I can't deal with her right now")))))
