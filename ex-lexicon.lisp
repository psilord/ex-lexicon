(in-package #:ex-lexicon)

(defparameter *db* (make-hash-table :test #'equal))

;; TODO: here just to make testing easier
(defparameter *lexographic-sort*
  '(;; A bare string is standalone sorting position,
    ;; Multiple strings in a () sort identically.  It
    ;; turns out, due to capitals, we always use the
    ;; latter form.
    ("A" "Á" "a" "á") ;; This group is < than the next
    ("Ch" "ch")
    ("E" "É" "e" "é")
    ("Ë" "ë")
    ("H" "h")
    ("Hw" "hw")
    ("I" "Í" "i" "í")
    ("K" "k")
    ("Kw" "kw")
    ("L" "l")
    ("M" "m")
    ("N" "n")
    ("O" "Ó" "o" "ó")
    ("p" "p")
    ("R" "r")
    ("S" "s")
    ("T" "t")
    ("U" "Ú" "u" "ú")
    ("V" "v") ;; last one
    ))


(defmacro deflex (lexeme options &body body)
  (let ((lex (gensym)))
    `(let ((,lex ,lexeme))
       (setf (gethash ,lex *db*)
             (list :options ',options
                   :body (cons 'entry (cons (list 'lexeme ,lex) ',body)))))))


;; This is specific to Kílta
;; TODO: Another poorly written function. :)
(defun explode-lexeme (lexeme)
  "RETURN an EXPLODE of the lexeme, but being conscious of digraphs."
  (let ((result nil)
        (len (length lexeme)))
    (flet ((digraph-p (pos)
             (let* ((digraph-db '("ch" "hw" "kw"))
                    (next-pos (1+ pos))
                    (next-pos-p (< next-pos len)))
               ;; NOTE: Lookup the lowercase, but return the actually found
               ;; digraph.  This preserves case in the string.
               (when next-pos-p
                 (let* ((digraph (subseq lexeme pos (1+ next-pos)))
                        (found-p
                          (member digraph digraph-db
                                  :test (lambda (left right)
                                          (string=
                                           (string-downcase left)
                                           (string-downcase right))))))
                   (when found-p digraph))))))
      (loop :for i :below len
            :for digraph = (digraph-p i)
            :do (cond
                  (digraph
                   (push digraph result)
                   (incf i))
                  (t
                   (pushnew (string (aref lexeme i)) result))))
      (nreverse result))))


(defun lexeme< (left-lexeme right-lexeme lexographic-sorting-order)
  ;; slow since it makes the constant hash all the time.  also slow since it
  ;; explodes the lexemes according to the sorting order elements.
  ;;
  ;; TODO: Prolly rewrite to do array comparison with two indices and subseq
  ;; picking.
  ;;
  ;; TODO: PRED can only be #'< or #'>
  (let* ((word-sort-hash (sort-word-hash lexographic-sorting-order))
         (left (explode-lexeme left-lexeme))
         (left-len (length left))
         (right (explode-lexeme right-lexeme))
         (right-len (length right)))

    (loop :for left-element :in left
          :for right-element :in right
          :for idx :by 1
          :do (let ((left-sort-val (gethash left-element word-sort-hash))
                    (right-sort-val (gethash right-element word-sort-hash)))
                (unless (= left-sort-val right-sort-val)
                  (cond
                    ((< left-sort-val right-sort-val)
                     (return-from lexeme< idx))
                    ((> left-sort-val right-sort-val)
                     (return-from lexeme< nil))))))

    (cond
      ((< left-len right-len)
       left-len)
      (t
       nil))))

(defun lexeme> (left-lexeme right-lexeme lexographic-sorting-order)
  ;; slow since it makes the constant hash all the time.  also slow since it
  ;; explodes the lexemes according to the sorting order elements.
  ;;
  ;; TODO: Prolly rewrite to do array comparison with two indices and subseq
  ;; picking.
  (let* ((word-sort-hash (sort-word-hash lexographic-sorting-order))
         (left (explode-lexeme left-lexeme))
         (left-len (length left))
         (right (explode-lexeme right-lexeme))
         (right-len (length right)))

    (loop :for left-element :in left
          :for right-element :in right
          :for idx :by 1
          :do (let ((left-sort-val (gethash left-element word-sort-hash))
                    (right-sort-val (gethash right-element word-sort-hash)))
                (unless (= left-sort-val right-sort-val)
                  (cond
                    ((> left-sort-val right-sort-val)
                     (return-from lexeme> idx))
                    ((< left-sort-val right-sort-val)
                     (return-from lexeme> nil))))))

    (cond
      ((> left-len right-len)
       right-len)
      (t
       nil))))


(defun lexeme= (left-lexeme right-lexeme lexographic-sorting-order)
  ;; slow since it makes the constant hash all the time.  also slow since it
  ;; explodes the lexemes according to the sorting order elements.
  ;;
  ;; TODO: Prolly rewrite to do array comparison with two indices and subseq
  ;; picking.
  (let* ((word-sort-hash (sort-word-hash lexographic-sorting-order))
         (left (explode-lexeme left-lexeme))
         (left-len (length left))
         (right (explode-lexeme right-lexeme))
         (right-len (length right)))

    (unless (= left-len right-len)
      (return-from lexeme= nil))

    (loop :for left-element :in left
          :for right-element :in right
          :for idx :by 1
          :do (let ((left-sort-val (gethash left-element word-sort-hash))
                    (right-sort-val (gethash right-element word-sort-hash)))
                (unless (= left-sort-val right-sort-val)
                  (cond
                    ((> left-sort-val right-sort-val)
                     (return-from lexeme= nil))
                    ((< left-sort-val right-sort-val)
                     (return-from lexeme= nil))))))

    t))



;;;; ---------------------------------------------------------------

;; Hrm, maybe I can use this in a pass from the DSL to an IR form to reorder
;; all the forms I need into the correct order before processing them for
;; emission.
(defparameter *policy/default*
  '(:default (:language "kílta"
              :pass nil

              :lexographic-sort
              (;; A bare string is standalone sorting position,
               ;; Multiple strings in a () sort identically.  It
               ;; turns out, due to capitals, we always use the
               ;; latter form.
               ("A" "Á" "a" "á") ;; This group is < than the next
               ("Ch" "ch")
               ("E" "É" "e" "é")
               ("Ë" "ë")
               ("H" "h")
               ("Hw" "hw")
               ("I" "Í" "i" "í")
               ("K" "k")
               ("Kw" "kw")
               ("L" "l")
               ("M" "m")
               ("N" "n")
               ("O" "Ó" "o" "ó")
               ("p" "p")
               ("R" "r")
               ("S" "s")
               ("T" "t")
               ("U" "Ú" "u" "ú")
               ("V" "v") ;; last one
               )

              :n-graphs ("ch" "hw" "kw")

              ;; page 90-93 and 183-185, and 187-190  are useful
              :languages ((vernacular v)
                          (english e)
                          (regional r)
                          (national n))


              ;; TODO: Needs work.
              :wierd-stuff ((entry 0 (lexeme
                                      (lexhead :groups etymology morphology)
                                      (senses :groups sense)
                                      (sublems :groups sublem)
                                      (sees :groups see)))
                            (exception 0 (v e r n))
                            (see 0 (v e r n))
                            (example 0 (v e r n ))
                            (sense 0 (label
                                      pos
                                      gloss
                                      definition
                                      usage
                                      (examples :groups example)
                                      (sees :groups see)))
                            (pos 0 (v e r n))
                            (gloss 0 (v e r n))
                            (definition 0 (v e r n))
                            (usage 0 (v e r n))
                            (sublem 0 (senses :group sense))
                            )

              :syntax ()



              ;; Just bookeeping junk for myself for now.
              :forms (
                      ;; \an: Antonym
                      ;; (takes all languages)
                      ;; (requires v)
                      ;; other languages get appended like gloss
                      (antonym (v "bar"))

                      ;; \bb: Biblio Ref
                      ;; (requires :e only)
                      (bibref (e "foo"))

                      ;; \bw: Borrowed Word
                      ;; (requires e only)
                      (borrow (e "Sanskrit"))

                      ;; \cf, \ce, \cn, \cr: Cross Reference
                      ;; (requires v)
                      ;; other languages get appended like gloss
                      (xref (v "foo"))

                      ;; \de: Definiton English
                      ;; \dv: Definition Vernacular
                      ;; \dr: Definition Regional
                      ;; \dn: Definition National
                      ;; (requires e), others languages appended like gloss
                      (definition (e "the definition"))

                      ;; \dt: Date
                      ;; can have multiple dates, including comments for a date
                      ;; using one or more languages.
                      (date
                       "01/01/2020"
                       ("01/01/2020" (e "optional comment in english")))

                      ))

    :main-dictionary (:inherit :default)


    ))



(defmacro generate-syntax-predicates (&body names)
  (let ((predicates
          (loop :for name :in names
                :collect
                `(defun ,(alexandria:symbolicate name '-p) (form)
                   (when (listp form)
                     (eq (first form) ',name))))))
    `(progn ,@predicates)))

;; defined or ONLY have syntax-p be available. Maybe grab it out of the passes?
;; TODO: Could prolly remove this along with the hand pass example.
(generate-syntax-predicates
  entry lexeme exception
  sublems sublem
  sees see
  senses sense label pos gloss definition usage
  examples example
  v e n r)

;; Syntax predicates for the human written DSL.
(defun syntax-p (name &optional form)
  (when form
    (when (listp form)
      (eq (first form) name))))

(defun syntax-p-pred (name)
  (lambda (form)
    (syntax-p name form)))

(defun syntax-p-preds (names)
  "Convert the list of syntax NAMES into a list of predicate functions that test
for each one."
  (mapcar (lambda (name) (syntax-p-pred name)) names))


;; Syntax predicates for the rule language used in the passes
(defun rule-syntax-p (name form)
  (when (consp form)
    (cond
      ((symbolp name)
       (eq name (first form)))
      ((consp name)
       (eq (first name) (first form))))))



(defun coalesce-syntax-forms (&rest forms)
  (reduce #'append forms))


(defun extract-fixed-args (args fixed-arg-num)
  (subseq args 0 fixed-arg-num))

(defun extract-varying-args (args fixed-arg-num)
  (subseq args fixed-arg-num))

;; THe identity function for a pass reshaping.
(defun pass/identity (cmd args pass)
  (declare (ignore pass))
  (cons cmd args))

;; subform-names is a list of symbols that identify the forms which will be
;; extracted and grouped into the group-name form. If there are multiple
;; group-name forms already in existence in forms (in addition so multiple
;; instances of the subforms), all of it will be lifted and canonicalized into a
;; single group form preserving order of all the subforms.
(defun group-subforms (rule group-name forms)
  (let (;; This is a pred for BOTH the group and subform names.
        (group-or-subform-pred (element-syntax-p-pred rule group-name))
        (grouped-forms nil))

    (dolist (form forms)
      (cond
        ;; Check to see if this form is already a group.  If it is, then check
        ;; if we should gather any subforms of that group.
        ((syntax-p group-name form)
         (dolist (subform (rest form)) ;; TODO: fixed-arg possibility?
           (when (funcall group-or-subform-pred subform)
             (push subform grouped-forms))))

        ;; Check to see if some random form in forms should be grouped.
        ;;
        ;; NOTE: We can use group-or-subform-pred here because if this form
        ;; was actually a group, the previous branch in the COND would have
        ;; caught it.
        ((funcall group-or-subform-pred form)
         (push form grouped-forms))))

    (list `(,group-name ,@(nreverse grouped-forms)))))

;; This implements the by hand stuff in the PASS function below.
(defun pass/reshape (cmd args pass)
  (let ((rule (rule pass cmd)))
    (unless rule
      ;; If there is no particular rule for this command,
      ;; just return the original form unadulterated
      (return-from pass/reshape (values cmd args)))

    (when (empty-p rule cmd)
      ;; If there was something specified, but it is empty, then
      ;; REMOVE the subforms since we've been explicitly told to!
      ;; However, keep the fixed arg!
      (return-from pass/reshape
        (values cmd (extract-fixed-args args (fixed-arg-num rule)))))

    ;; Otherwise, we enact the reshape in question
    (let ((db (make-hash-table))
          (fixed-args (extract-fixed-args args (fixed-arg-num rule)))
          (varying-args (extract-varying-args args (fixed-arg-num rule))))

      ;; Collect all specified subforms. For subforms that represent groups
      ;; descriptions, perform the collecting and then group the subforms
      ;; too. We dont specifcally need the order right here, but it is a
      ;; convenient way to get all the reshape keys.
      ;;
      ;; NOTE: Implicitly handles :unspecified.
      ;; NOTE: Be careful of the :or syntax.
      (dolist (elem (order rule))
        (let ((elem
                (if (rule-syntax-p :or elem)
                    (loop :for form :in (rest elem)
                          ;; find the first form which has results and use that
                          ;; one.
                          ;;
                          ;; TODO: Clean this up!
                          :do (let ((ans (find-all-if
                                          (element-syntax-p-pred rule form)
                                          varying-args)))
                                ;; May return nothing for all (rest elem)
                                (when ans
                                  (return form))))
                    elem)))
          ;; this handles if the form simply didn't exist in the varying-args
          (when elem
            (setf (gethash elem db)
                  (let ((forms (find-all-if (element-syntax-p-pred rule elem)
                                            varying-args)))
                    (if (group-p rule elem)
                        (group-subforms rule elem forms)
                        forms))))))

      ;; Then return as values the reshaped cmd and processed args
      ;; in the order specified in the rule. Be careful of the :or forms
      ;; in the ordering.
      (let ((new-args
              (apply #'coalesce-syntax-forms
                     (loop :for i :in (order rule)
                           :when (symbolp i)
                             :collect (gethash i db)
                           :when (rule-syntax-p :or i)
                             :collect ;; TODO: Clean this up
                             (loop :for form :in (rest i)
                                   :do (multiple-value-bind (form presentp)
                                           (gethash form db)
                                         (when presentp
                                           (return form))))))))

        (values cmd (append fixed-args new-args))))))



(defclass reshape-rule ()
  (;; The name of a single rewrite rule (in some pass).
   (%name :accessor name
          :initarg :name)
   ;; How many fixed args this rewrite rule specifies
   (%fixed-arg-num :accessor fixed-arg-num
                   :initarg :fixed-arg-num)
   ;; The ordering of the resultant rewrites.
   (%order :accessor order
           :initarg :order)

   ;; The actual dsl reshape rules for each subform or group form.  Keyed by
   ;; name of the subform/group-name, Value is NIL for a bare subform name and a
   ;; list of subform names for a group name.
   ;;
   ;; TODO: If these rules get more complicated, these might need abstraction.
   (%reshapes :accessor reshapes
              :initarg :reshapes
              :initform (make-hash-table))

   ;; If there is an :unspecified element in a group, we record the
   ;; group name in which it exists here.
   (%unspecified-group-name :accessor unspecified-group-name
                            :initarg :unspecified-group-name
                            :initform nil)

   ;; syntax predicates, keyed by subform name or group name (which cannot
   ;; intersect).
   (%preds-db :accessor preds-db
              :initarg :preds-db
              :initform (make-hash-table))))


;; WHen the reshape is empty, it means to drop everything in the subforms,
;; except the fixed arguments.
(defmethod empty-p ((rule reshape-rule) cmd)
  (zerop (hash-table-count (reshapes rule))))

;; TODO: Think of a better name for these next two methods.
(defmethod element-reshape-form ((rule reshape-rule) element)
  ;; The value of this form indicates what kinds of reshaping needs to happen.
  (gethash element (reshapes rule)))

(defmethod (setf element-reshape-form) (form (rule reshape-rule) element)
  (setf (gethash element (reshapes rule)) form))

(defmethod element-syntax-p-pred ((rule reshape-rule) element)
  (gethash element (preds-db rule)))

(defmethod (setf element-syntax-p-pred) (pred (rule reshape-rule) element)
  (setf (gethash element (preds-db rule)) pred))

(defmethod group-p ((rule reshape-rule) elem)
  ;; Currently this will return NIL for non-groups, and some list of forms
  ;; for elements that are group designators.
  (element-reshape-form rule elem))

;;;; NOTE: The functions below could be converted into a general tree traversal
;;;; if I initially converted the rule DSL into a true AST.  Then I could use
;;;; methods on the AST node type to perform each rule processing pass. If this
;;;; rule syntax and semantics gets more complex, I'll have to do that. But for
;;;; now, each rule reshaper pass replicates the COND that deals with the types
;;;; in order to process the rule DSL (much like old school C compilers).

(defun map-reshaper-elements (func reshaper &key collect-nil flatten)
  "Map the FUNC across the elements of the RESHAPER form and collect
the results. If :COLLECT-NIL is false, then if the FUNC returns a NIL
don't collect it. And if :FLATTEN is true, if the FUNC has returned a list
into the accumulating result, when the mapping process is over, flatten the
result list."
  (let ((results nil))
    (loop :for element :in reshaper
          :do (let ((result (funcall func element)))
                (if collect-nil
                    (push result results)
                    (when result
                      (push result results)))))
    (funcall (if flatten #'alexandria:flatten #'identity)
             (nreverse results))))

(defun gather-valid-element-names (reshaper)
  "In the reshaper portion, find all valid element names (which include group
names). Return the list."
  (map-reshaper-elements
   (lambda (element)
     (cond
       ((member element '(:unspecified :or))
        nil)

       ;; Handle the :or rule form
       ((rule-syntax-p :or element)
        (gather-valid-element-names (rest element)))

       ((symbolp element)
        element)

       ((rule-syntax-p :group element)
        ;; we want the group name, and all names in the grouping form
        ;; (which cannot be anything other than bare names)
        (cons (second element)
              (remove-if (is-eq :unspecified) (third element))))))

   reshaper
   :flatten t))

(defun find-group-using-unspecified (reshaper)
  (let ((group-name-with-unspecified
          (map-reshaper-elements
           (lambda (element)
             (cond
               ((rule-syntax-p :group element)
                ;; save off which group-name contained the :unspecified
                (when (member :unspecified (third element))
                  (second element)))
               ((rule-syntax-p :or element)
                (find-group-using-unspecified (rest element)))))
           reshaper
           :flatten t)))

    (unless (zero-or-one-p (length group-name-with-unspecified))
      (error "Only one group name in a reshaper may use :unspecified!"))

    (first group-name-with-unspecified)))

(defun determine-final-element-order (reshaper)
  ;; This includes not only the bare name, but also ONLY the group names and
  ;; any bare/group name in an :or form. It discovers them in a left to right
  ;; order as found in the reshaper.
  (let* ((unoptimized-ordering
           ;; Specifically, don't flatten beause we handle the :or forms
           ;; specially.
           (map-reshaper-elements
            (lambda (element)
              (cond
                ((eq element :unspecified)
                 :unspecified)

                ((rule-syntax-p :or element)
                 (cons :or (determine-final-element-order (rest element))))

                ((symbolp element)
                 element)

                ((rule-syntax-p :group element)
                 (second element))))
            reshaper))

         (optimized-ordering
           ;; Now, any nested :or forms are flattened as this represents a left
           ;; to right optimization of the :or form (which is left
           ;; associative). We need to do this in order to make it much easier
           ;; to determine how to apply the :or form when actually ordering the
           ;; elements into the final form.
           (map-reshaper-elements
            (lambda (element)
              (if (rule-syntax-p :or element)
                  (cons :or (remove-if (is-eq :or)
                                       (alexandria:flatten element)))
                  element))
            unoptimized-ordering)))

    optimized-ordering))

(defun type-reshaper-elements (rule reshaper)
  ;; For each element in the reshaper, store a note about what kind of element
  ;; it is. Note: This might get more complex in order to mark how the type of
  ;; the elements in the reshaper. I should really also mark when I see
  ;; unspecified among other things like that too so I don't have to explicitly
  ;; search for it in the element form.
  ;;
  ;; TODO: Try and implement some of the above.
  ;;
  ;; Currently:
  ;; bare element name -> nil
  ;; group name -> subforms.
  (map-reshaper-elements
   (lambda (element)
     (cond
       ((eq element :unspecified)
        nil)

       ((rule-syntax-p :or element)
        (type-reshaper-elements rule (rest element)))

       ((symbolp element)
        ;; NIL means it isn't a group. :)
        (setf (element-reshape-form rule element) nil))

       ((rule-syntax-p :group element)
        ;; The value is the subform list, which we peruse later.
        (let ((group-name (second element))
              (subform-names (third element)))
          (setf (element-reshape-form rule group-name) subform-names)))))
   reshaper))

(defun synthesize-syntax-predicates (rule reshaper)
  ;; For each element in the reshaper, generate a syntax predicate that
  ;; matches what that element is representing. Be very careful when handling
  ;; a group that contains :unspecified.
  (map-reshaper-elements
   (lambda (element)
     (cond
       ((eq element :unspecified)
        nil)

       ((rule-syntax-p :or element)
        (synthesize-syntax-predicates rule (rest element)))

       ((symbolp element)
        (setf (element-syntax-p-pred rule element) (syntax-p-pred element)))

       ((rule-syntax-p :group element)
        (let ((group-name (second element))
              (subform-names (third element)))
          (setf (element-syntax-p-pred rule group-name)
                (apply #'one-of
                       ;; We tack on the :unspecified predicate if we notice
                       ;; this specific :groups form contained it.
                       (append
                        (syntax-p-preds
                         ;; Ensure to discover BOTH group-name and
                         ;; subform-name forms.
                         (cons group-name
                               (remove-if (is-eq :unspecified)
                                          subform-names)))
                        ;; and if this group name happens to be the one which
                        ;; contained the :unspecified, union in the
                        ;; unspecified predicate.
                        (when (eq group-name (unspecified-group-name rule))
                          (list (element-syntax-p-pred rule
                                                       :unspecified))))))))))
   reshaper))


(defun make-reshape-rule (name fixed-arg-num reshaper)
  (let* ((rule (make-instance 'reshape-rule :name name
                                            :fixed-arg-num fixed-arg-num))
         (unspecified-count (count :unspecified (alexandria:flatten reshaper)))
         (unspecified-valid-p (zero-or-one-p unspecified-count)))

    (unless unspecified-valid-p
      (error "MAKE-RESHAPE-RULE: In rule name ~S, :unspecified was used more than once." name))

    ;; NOTE: The order of these steps matter since later steps use previous
    ;; step's output.

    ;; First, we collect all group-names and bare names so we can create
    ;; the :unspecified predicate. We build this even if we don't use it since
    ;; it can come in handy in other places.
    (let ((all-bare/group-names (gather-valid-element-names reshaper)))

      ;; Then, if :unspecified in is a group, find the group it is in.
      (setf (unspecified-group-name rule)
            (find-group-using-unspecified reshaper))

      ;; Then, build the :unspecified predicate for this rule. This will
      ;; match the complement of all known bare and group names in this rule.
      (setf (element-syntax-p-pred rule :unspecified)
            (complement
             (if all-bare/group-names
                 ;; Match anything that WON'T match these names.
                 (apply #'one-of (syntax-p-preds all-bare/group-names))
                 ;; If there is no :unspecified, then we match NOTHING.
                 (constantly t))))

      ;; Then, get the final element ordering from the reshaper.
      (setf (order rule) (determine-final-element-order reshaper))

      ;; Then, compute the element types and store any required data picked
      ;; from the reshaper elements.
      (type-reshaper-elements rule reshaper)

      ;; Then, build the predicates that match each reshaper depending if it
      ;; is a bare name or a group-name.
      (synthesize-syntax-predicates rule reshaper)

      rule)))


(defclass pass ()
  ((%pass-func :accessor pass-func
               :initarg :pass-func
               :initform #'pass/identity)
   (%rule-db :accessor rule-db
             :initarg :rule-db
             ;; keyed by rule name, value is a rule instance
             :initform (make-hash-table))))

(defmethod rule ((pass pass) name)
  (gethash name (rule-db pass)))

(defmethod (setf rule) (new-rule (pass pass) name)
  (setf (gethash name (rule-db pass)) new-rule))

(defun make-pass (&rest args)
  (apply #'make-instance 'pass args))

(defun make-reshape-pass (pass-descriptor)
  (let ((pass (make-pass :pass-func #'pass/reshape)))
    (dolist (rule pass-descriptor)
      (destructuring-bind (cmd fixed-arg-num reshaper) rule
        (let ((reshape-rule (make-reshape-rule cmd fixed-arg-num reshaper)))
          (setf (rule pass (name reshape-rule)) reshape-rule))))
    pass))




;; This is a higher order intermediate representation form rewriting system,
;; very similar to how a compiler rewrites forms into other forms closer to what
;; you need it to be. If you squint, it is actually a metacircular evaluator
;; with the evaluation functionality being higher order.
(defun mappass-list (strm forms pass env)
  (mapcar (lambda (form)
            (mappass strm form pass env))
          forms))

(defun mappass (strm lexform pass env)
  (cond
    ((atom lexform)
     lexform)
    (t
     (destructuring-bind (cmd &rest args) lexform
       (multiple-value-bind (new-cmd new-args)
           (funcall (pass-func pass) cmd args pass)
         (cons new-cmd
               (mappass-list strm new-args pass env)))))))

(defun doit3 (&optional (lexeme "á"))
  (let* ((entry (getf (gethash lexeme *db*) :body))
         ;; NOTE: The reshape portion is done _in order_.
         ;;
         ;; NOTE: If say in entry there are forms in the dsl that you do not
         ;; specify here, they will be DROPPED. (Is this what I want?)
         ;;
         ;; NOTE: A number of fixed-args are supported between the cmd and
         ;; the tail of other forms associated with that cmd.
         ;;
         ;; Determine if this is the behavior I'd want or expect. As in, should
         ;; all forms not otherwise mentioned be kept and only those indicated
         ;; reordered? Where would I specify the unordered forms go, before,
         ;; after, something else?  and if I say wanted to just rewrite for
         ;; english, then writing something like '((see (v e)) (gloss (v e))
         ;; (definition (v e)) etc) would be very straightforward--as opposed to
         ;; specifying EVERYTHING along with those few changes to extract out
         ;; the v and e parts.
         ;;
         ;; (cmd 0-or-more-fixed-args &rest args)
         (canon-pass
           ;; Canonicalize the human written form and put anything
           ;; unspecified into a specific place in each rule's reshaper.
           ;; NOTE: There can only be ONE use of :unspecified in each rule.
           (make-reshape-pass
            '((entry 0 (lexeme ;; Don't use name more than once!
                        ;; TODO: Support :or in the group subform list when
                        ;; needed. This feature isn't supported now.
                        (:group homonyms (homonym))
                        (:group headers (etymology
                                         morphology
                                         pronounciation))
                        (:group senses (sense))
                        (:group sublems (sublem))
                        (:group sees (see))
                        #++ (:group xxx (:unspecified))

                        ;; Keep the first one found.
                        ;; Supports both groups and bare names (and intermixing)
                        #++(:or
                            (:group yyy (wumpscut))
                            (:group zzz (froboz)))
                        :unspecified
                        ))

              (homonym 0 ((:group headers (etymology
                                           morphology
                                           pronounciation))
                          (:group senses (sense))
                          (:group sublems (sublem))
                          (:group sees (see))))

              (etymology 0 (v e r n :unspecified))
              (morphology 0 (v e r n :unspecified))
              (pronounciation 0 (ipa :unspecified))
              (see 0 ((:group refs (ref))
                      :unspecified))
              (example 0 (v e r n :unspecified))
              (sense 0 (label
                        pos
                        gloss
                        definition
                        usage
                        (:group examples (example))
                        (:group sees (see))
                        :unspecified))
              (pos 0 (v e r n :unspecified))
              (gloss 0 (v e r n :unspecified))
              (definition 0 (v e r n :unspecified))
              (usage 0 (v e r n :unspecified))
              (sublem 1 ((:group headers (etymology
                                          morphology
                                          pronounciation))
                         (:group senses (sense))
                         (:group sublems (sublem))
                         (:group sees (see))
                         :unspecified))

              ;; Groups (even implicitly defined ones like the
              ;; above) can be ordered too! (if there are more
              ;; than one kind of subform in it, that is.)
              (headers 0 (etymology morphology pronounciation :unspecified))

              ;; Do I ever need this next idea? Currently if you don't specify
              ;; an element form in here it is just passed as identity. This
              ;; makes an alteration to that.
              ;;
              ;; The are terminal-ish forms in the recursion.
              ;; These require AT LEAST ONE argument (string, form, etc).
              ;; :any means preserve whatever you find there in the
              ;; same order you found it.
              #++(v 1 (:any))
              #++(e 1 (:any))
              #++(r 1 (:any))
              #++(n 1 (:any))
              #++(ref 0 (:any))

              )))

         (passes (list canon-pass))

         (result nil))

    ;; Each pass applied is basically reordering, or subtractive.
    ;; TODO: Add in command renaming, but do I need it?
    (loop :for pass :in passes
          :do (setf result (mappass t entry pass nil)))

    result))


;; New attempt.

(defun doit4 (&optional (lexeme "á"))
  (let* ((entry (getf (gethash lexeme *db*) :body))
         (canon-pass
           ;; make-new-reshape-pass
           '(
             ;;-----------------------------
             :define-reshape-fixed-args-defaults
             ((sublem 1))

             ;;-----------------------------
             :define-rewrites
             ;; Maybe allow giving a name here so I can have different group
             ;; rules that do the same thing, but maybe :unspecified got added.
             ;;
             ;; Example: (:group dname group-name (subform-names))
             ;; 'dname' is a disambiguating name for the group name and subforms
             ;; 'group-name' is the actual symbol found in the dict dsl.
             ;; 'subform-name' are the actual symbol in the dict dsl subforms.

             ;; :identity means don't disturb the number, layout, ordering, or
             ;; anything else about the subforms in the group.
             ;;
             ;; Also, these are the "ground forms" in the dict dsl in that
             ;; we don't want to reshape anything about the inside of these
             ;; forms. They are basically the dict dsl leaf forms.
             ;;
             ;;
             ;; NOTE: (MAYBE) Support :or forms in subform list of groups
             ((:group lexeme lexeme (:identity))
              (:group v v (:identity))
              (:group e e (:identity))
              (:group r r (:identity))
              (:group n n (:identity))
              (:group ref ref (:identity))
              (:group ref-u ref (:identity :unspecified))
              (:group label label (:identity))
              (:group label-u label (:identity :unspecified))

              ;; Now we start the aggregation rules.
              (:group refs refs (ref))
              (:group refs-u refs (ref :unspecified))
              (:group pos pos (v e r n))
              (:group pos-u pos (v e r n :unspecified))
              (:group usage usage (v e r n))
              (:group usage-u usage (v e r n :unspecified))
              (:group definition definition (v e r n))
              (:group definition-u definition (v e r n :unspecified))
              (:group example example (v e r n))
              (:group example-u example (v e r n :unspecified))
              (:group examples examples (example))
              (:group examples-u examples (example :unspecified))
              (:group sense sense (label pos gloss definition usage examples
                                   sees))
              (:group sense-u sense (label pos gloss definition usage examples
                                     sees :unspecified))
              (:group senses senses (sense))
              (:group senses-u senses (sense :unspecified))
              (:group sublem sublem (senses))
              (:group sublem-u sublem (senses :unspecified))
              (:group sublems sublems (sublem))
              (:group sublems-u sublems (sublem :unspecified))
              (:group headers headers (etymology morphology pronounciation))
              (:group headers-u headers (etymology morphology pronounciation
                                         :unspecified))
              (:group homonym homonym (headers senses sublems))
              (:group homonym-u homonym (headers senses sublems :unspecified))
              (:group homonyms homonyms (homonym))
              (:group homonyms-u homonyms (homonym :unspecified))
              )

             ;; ----------------------------
             :reshape-rules
             ((entry 0
               ;; NOTE: Specifying the below overrides the generics above just
               ;; for the individual thing you redefined:
               ;; :fixed-args (junkish 1)

               ;; If :unspecified is NOT used, all non-kept forms are DROPPED.
               ;; NOTE: Generate predicates, for :unspecified too. Use sieve to
               ;; actually filter. order doesn't matter here.
               :observe (;; NOTE: These are actual group names, NOT dnames.
                         lexeme
                         homonym
                         homonyms
                         headers
                         etymology
                         morphology
                         pronounciation
                         sense
                         senses
                         sublem
                         sublems
                         ref
                         refs
                         ;; This represents :unspecified AT THIS LEVEL of ENTRY
                         ;; form
                         :unspecified)

               ;; group rewrites happens in the left-to-right order
               ;; specified. This allows lifting of sense forms into senses
               ;; forms if the latter wasn't specified at all and THEN group
               ;; THOSE into homonym and homonym forms, for example.
               ;;
               ;; At the end of this, all individual groups are coalesced into a
               ;; single group, even if there were several of them.
               ;;
               ;; NOTE: You can override a group definition in the
               ;; :group-definitions section by simply using (:group x (a b c))
               ;; in the form below in place of a gorup name.
               ;;
               ;; NOTE: The override allows :or and :unspecified in the right
               ;; hand side of the group. If :unspecified is used, it can only
               ;; be used ONCE in the :groups.
               :rewrite-sequence (refs senses sublems headers homonym
                                  homonyms)

               ;; Then finally, this is the ordering of the resultant groupings.
               ;; Every form in here must be a root in :groupings. All
               ;; :groupings group names must be accounted for in here.
               :keep-order (lexeme
                            ;; TODO: Support :or forms in here
                            homonyms
                            sees
                            :unspecified))

              ;; ----------------------------
              (homonyms 0
               ;; specify below if I need to override the defaults
               ;;:fixed-args ((sublem 1))
               :observe (homonym
                         headers
                         etymology
                         morphology
                         pronounciation
                         sense
                         senses
                         sublem
                         sublems
                         ref
                         refs
                         :unspecified)
               :rewrite-sequence (ref refs sense senses sublem sublems headers
                                  homonym)
               :keep-order (homonym
                            refss
                            :unspecified))

              ;; ----------------------------
              (headers 0
               :fixed-args nil
               :observe (etymology
                         morphology
                         pronounciation)
               :rewrite-sequence :todo
               :keep-order :todo)

              )))
         (passes (list canon-pass))

         (result nil))

    ;; Each pass applied is basically reordering, or subtractive.
    ;; TODO: Add in command renaming, but do I need it?
    (loop :for pass :in passes
          :do (setf result (mappass t entry pass nil)))

    result))
