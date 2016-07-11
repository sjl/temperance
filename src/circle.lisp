(in-package #:bones.circle)

;;;; Circular, Doubly-Linked Lists
;;; If you found this through a Google search or a link or something, turn back
;;; now.  You probably don't want this.
;;;
;;; When we're creating and optimizing the WAM instructions (before rendering
;;; into bytecode) it would be really nice to have a data structure that
;;; supports a few things:
;;;
;;; * O(1) appending (when building the initial list of instructions)
;;; * O(n) forward traversal (when rendering and optimizing for `set_constant`)
;;; * O(n) backward traversal (when optimizing for `unify_constant`)
;;; * In-place removal/replacement, with the ability to choose "which side we
;;;   move to" after.
;;;
;;; That last one is tricky.  We want to be able to remove/replace/splice
;;; elements during a single traversal.  For example, we want to perform the
;;; following optimization for constants (taken from the WAM book erratum):
;;;
;;;     unify_variable Xi     -> unify_constant c
;;;     ...
;;;     get_structure c/0, Xi
;;;
;;; The nicest way to do this would be something like:
;;;
;;; 1. Iterate backward from end to start.
;;; 2. When we see a `:get-structure-* CONSTANT LOCALREG` instruction:
;;;   A. Remove it in-place, so the next node will be processed on the next
;;;      iteration (remember, we're iterating backwards).
;;;   B. Search forward for the corresponding `:subterm-variable` instruction
;;;      and replace it in-place with the `:subterm-constant` instruction.
;;;
;;; Of course you could do all this with immutable data structures, but it'll be
;;; pretty slow.  And since one of the primary goals of this project is to be
;;; fast, we don't want to do slow things.
;;;
;;; So instead we make our own data structure for the list of WAM instructions.
;;; A "circle" is a circular, doubly-linked list, with a sentinel node to denote
;;; the start/end of the list.
;;;
;;; TODO: explain further
;;; TODO: docstrings below

(defconstant +circle-sentinel+ 'circle-sentinel)


(declaim (inline circle-prev circle-value circle-next
                 make-circle make-circle-between
                 circle-tie circle-sentinel-p))

(defstruct circle prev value next)


(defun* circle-tie ((c1 circle) (c2 circle))
  (setf (circle-next c1) c2
        (circle-prev c2) c1))


(defun* make-empty-circle ()
  (:returns circle)
  "Create an empty circle.

  It will still contain a sentinel.

  "
  (let ((circle (make-circle :value +circle-sentinel+)))
    (setf (slot-value circle 'prev) circle
          (slot-value circle 'next) circle)
    circle))

(defun* make-circle-with ((list list))
  "Create a circle whose nodes contain the values in `list`."
  (:returns circle)
  (let ((sentinel (make-empty-circle)))
    (loop :with prev = sentinel
          :for value :in list
          :for current = (make-circle :prev prev
                                      :value value)
          :do (setf (circle-next prev) current
                    prev current)
          :finally (unless (null list)
                     (circle-tie current sentinel)))
    sentinel))

(defun* make-circle-between ((left circle) value (right circle))
  ;; L new R
  (let ((c (make-circle :prev left
                        :value value
                        :next right)))
    (setf (circle-next left) c
          (circle-prev right) c)
    c))


(defun* circle-sentinel-p ((circle circle))
  (:returns boolean)
  "Return whether this circle node is the sentinel."
  (eq (circle-value circle) +circle-sentinel+))

(defun* circle-empty-p ((circle circle))
  (:returns boolean)
  "Return whether this circle is empty."
  (and (circle-sentinel-p circle)
       (eql circle (circle-next circle))))


(defun* circle-rotate ((circle circle) (n integer))
  (:returns circle)
  (cond
    ((> n 0) (circle-rotate (circle-next circle) (1- n)))
    ((< n 0) (circle-rotate (circle-prev circle) (1+ n)))
    (t circle)))

(defun* circle-nth ((circle circle) (n integer))
  (:returns circle)
  (when (not (circle-sentinel-p circle))
    (error "Can only call circle-nth on the sentinel."))
  (circle-rotate circle
                 (if (< n 0)
                   n
                   (1+ n))))


(defun* circle-insert-before ((circle circle) value)
  ;; L new old R
  (let ((old circle)
        (l (circle-prev circle)))
    (make-circle-between l value old)))

(defun* circle-insert-after ((circle circle) value)
  ;; L old new R
  (let ((old circle)
        (r (circle-next circle)))
    (make-circle-between old value r)))


(defun* circle-insert-beginning ((circle circle) value)
  (when (not (circle-sentinel-p circle))
    (error "Can only insert-beginning at the sentinel."))
  (circle-insert-after circle value))

(defun* circle-insert-end ((circle circle) value)
  (when (not (circle-sentinel-p circle))
    "Can only insert-end at the sentinel.")
  (circle-insert-before circle value))


(defun* circle-prepend-circle ((circle circle) (other circle))
  (when (not (circle-sentinel-p circle))
    (error "Can only prepend to the sentinel."))
  (when (not (circle-sentinel-p other))
    (error "Can only prepend from the sentinel."))
  ;; S new-first ... new-last R
  (let ((s circle)
        (r (circle-next circle)))
    (circle-tie s (circle-next other))
    (circle-tie (circle-prev other) r)))

(defun* circle-prepend ((circle circle) values)
  (unless (null values)
    (circle-prepend-circle circle (make-circle-with values))))


(defun* circle-append-circle ((circle circle) (other circle))
  (when (not (circle-sentinel-p circle))
    (error "Can only append to the sentinel."))
  (when (not (circle-sentinel-p other))
    (error "Can only append from the sentinel."))
  ;; L new-first ... new-last S
  (let ((s circle)
        (l (circle-prev circle)))
    (circle-tie l (circle-next other))
    (circle-tie (circle-prev other) s)))

(defun* circle-append ((circle circle) values)
  (unless (null values)
    (circle-append-circle circle (make-circle-with values))))


(defun* circle-forward ((circle circle))
  (:returns (or circle null))
  (let ((next (circle-next circle)))
    (when (not (circle-sentinel-p next))
      next)))

(defun* circle-backward ((circle circle))
  (:returns (or circle null))
  (let ((prev (circle-prev circle)))
    (when (not (circle-sentinel-p prev))
      prev)))


(defun* circle-remove ((circle circle))
  ;; L rem R
  (when (circle-sentinel-p circle)
    (error "Cannot remove sentinel."))
  (let ((l (circle-prev circle))
        (r (circle-next circle)))
    (circle-tie l r)))

(defun* circle-backward-remove ((circle circle))
  (:returns (or circle null))
  (prog1
      (circle-backward circle)
    (circle-remove circle)))

(defun* circle-forward-remove ((circle circle))
  (:returns (or circle null))
  (prog1
      (circle-forward circle)
    (circle-remove circle)))


(defun* circle-replace ((circle circle) value)
  (:returns circle)
  (when (circle-sentinel-p circle)
    (error "Cannot replace sentinel."))
  ;; L new R
  (let ((l (circle-prev circle))
        (r (circle-next circle)))
    (make-circle-between l value r)))

(defun* circle-backward-replace ((circle circle) value)
  (:returns (or circle null))
  (prog1
      (circle-backward circle)
    (circle-replace circle value)))

(defun* circle-forward-replace ((circle circle) value)
  (:returns (or circle null))
  (prog1
      (circle-forward circle)
    (circle-replace circle value)))


(defun* circle-splice ((circle circle) values)
  (if (null values)
    (circle-remove circle)
    (progn
      (when (circle-sentinel-p circle)
        (error "Cannot splice sentinel."))
      ;; L new-first ... new-last R
      (let ((l (circle-prev circle))
            (r (circle-next circle))
            (new (make-circle-with values)))
        (circle-tie l (circle-next new))
        (circle-tie (circle-prev new) r)))))

(defun* circle-backward-splice ((circle circle) values)
  (:returns (or circle null))
  (prog1
      (circle-backward circle)
    (circle-splice circle values)))

(defun* circle-forward-splice ((circle circle) values)
  (:returns (or circle null))
  (prog1
      (circle-forward circle)
    (circle-splice circle values)))


(defun* circle-to-list ((circle circle) &optional include-sentinel-p)
  (:returns list)
  (loop
    :with node = circle
    :when (or include-sentinel-p
              (not (circle-sentinel-p node)))
    :collect (circle-value node) :into results
    :do (setf node (circle-next node))
    :when (eql node circle) :do (return results)))


(defmethod print-object ((object circle) stream)
  (print-unreadable-object (object stream :type t :identity nil)
    (format stream "~S" (subst '%%% +circle-sentinel+ (circle-to-list object t)))))

