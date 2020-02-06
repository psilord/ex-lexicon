(in-package #:ex-lexicon)

(defun dbg (strm env policy fmt &rest args)
  (apply #'format strm (concatenate 'string
                                    "DBG: [env=~s policy=~S]~%  " fmt "~&")
         env policy args))

(defun find-all-if (pred sequ &rest keyword-args &key &allow-other-keys)
  (apply #'remove-if (complement pred) sequ keyword-args))

(defun sort-word-hash (word-sort)
  "Convert the list form of LEXOGRAPHIC-SORTING-ORDER into a hash table for
faster and more agreeable code for lookups."
  ;; key is letter/digraph, value is sorting position.
  (let ((word-sort-hash (make-hash-table :test #'equal)))
    (loop :for element/group :in word-sort
          :for idx :by 1
          :do (cond
                ((consp element/group)
                 (loop :for element :in element/group
                       :do (setf (gethash element word-sort-hash) idx)))
                ((stringp element/group)
                 (setf (gethash element/group word-sort-hash) idx))
                (t
                 (error
                  "SORT-WORD-HASH: Don't know how to sort thing: ~S"
                  element/group))))
    word-sort-hash))

(defun sort-forms (forms form-sorting-order)
  "Reorder the FORMS according to FORM-SORTING-ORDER. Example:
if FORM-SORTING-ORDER is '(a c b e d f)
and FORMS is '((a ...) (b ...) (c ...) (d ...) (e ...) (f ...))
then the result will be:
 '((a ...) (c ...) (b ...) (e ...) (d ...) (f ...))

It is an error to attempt sorting a symbol not in FORM-SORTING-ORDER.
"
  ;; Decorate the form-sorting-order in a left to right manner
  (let ((decorated-form-order
          (loop :for sym :in form-sorting-order
                :for idx :by 1
                :collecting (list sym idx))))
    (flet ((when-sort-error (sym)
             (unless sym
               (error "SORT-FORMS: Unable to sort sym ~S using form-order: ~S"
                      sym form-sorting-order))))

      ;; sort the forms by the decorated sorting order.
      ;; slow, but until I need it to be faster, good enough.
      (stable-sort forms
                   (lambda (l r)
                     (let ((l-sym-idx (second (assoc l decorated-form-order)))
                           (r-sym-idx (second (assoc r decorated-form-order))))
                       (when-sort-error l-sym-idx)
                       (when-sort-error r-sym-idx)
                       (< l-sym-idx r-sym-idx)))
                   :key #'first))))



(defun sieve (pred sequ &key (key #'identity)
                          (values t)
                          (pred-range-sort (constantly nil))
                          (initial-key-pool nil) ;; ensure all buckets present!
                          (result-transformer-func #'identity)
                          (decorate-position nil))

  (let ((result (make-hash-table :test #'equal)))
    ;; Initialize the key pool if supplied.
    (when initial-key-pool
      (dolist (initial-key initial-key-pool)
        (setf (gethash initial-key result) nil)))

    (flet ((separator-func (elem pos)
             (let ((decision (funcall pred (funcall key elem))))
               (let ((presentp (nth-value 1 (gethash decision result))))
                 (unless presentp
                   (setf (gethash decision result) nil))
                 (push (if decorate-position
                           (list pos elem)
                           elem)
                       (gethash decision result))))))

      (loop :for elem :in sequ
            :for pos :by 1
            :do (separator-func elem pos))

      (let ((result-list nil))
        (maphash (lambda (k v)
                   (push (list k (nreverse v)) result-list))
                 result)
        (let* ((sorted-result-list
                 (stable-sort result-list pred-range-sort :key #'first))
               (transformed-result-list
                 (mapcar (lambda (entry)
                           (list (first entry)
                                 (funcall result-transformer-func
                                          (second entry))))
                         sorted-result-list)))
          (if values
              (values-list transformed-result-list)
              transformed-result-list))))))


;; nil < t
(defun bool< (l r)
  (cond
    ((and (not l) (not r)) nil)
    ((and (not l) r) t)
    ((and l (not r)) nil)
    ((and l r) nil)))

;; t > nil
(defun bool> (l r)
  (cond
    ((and (not l) (not r)) nil)
    ((and (not l) r) nil)
    ((and l (not r)) t)
    ((and l r) nil)))

;; t == t, and nil == nil
(defun bool= (l r)
  (cond
    ((and l r) t)
    ((and (eq l nil) (eq r nil)) t)
    (t nil)))

(defun one-of (&rest funcs)
  (lambda (form)
    (block anon-func
      (dolist (func funcs)
        (when (funcall func form)
          (return-from anon-func t))))))

(defun all-of (&rest funcs)
  (lambda (form)
    (block anon-func
      (dolist (func funcs)
        (unless (funcall func form)
          (return-from anon-func nil)))
      t)))

(defun is-eq (x)
  (lambda (y)
    (eq x y)))

(defun is-eql (x)
  (lambda (y)
    (eql x y)))

(defun is-equal (x)
  (lambda (y)
    (equal x y)))

(defun is-equalp (x)
  (lambda (y)
    (equalp x y)))

(defun zero-or-one-p (value)
  (or (zerop value) (= value 1)))
