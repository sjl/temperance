(in-package #:bones.paip)

;;;; Types
(deftype logic-variable ()
  'keyword)

(deftype binding ()
  '(cons logic-variable t))

(deftype binding-list ()
  '(trivial-types:association-list keyword t))


;;;; Constants
(define-constant fail nil
  :documentation "Failure to unify")

(define-constant no-bindings '((:bones-empty-bindings . t))
  :test 'equal
  :documentation "A succesful unification, with no bindings.")

(defparameter *check-occurs* t
  "Whether to perform an occurs check.")


;;;; Unification
(defun* variable-p (term)
  (:returns boolean)
  "Return whether the given term is a logic variable."
  (keywordp term))

(defun* get-binding ((variable logic-variable)
                     (bindings binding-list))
  (:returns (or binding null))
  "Return the binding (var . val) for the given variable, or nil."
  (assoc variable bindings))

(defun* binding-variable ((binding binding))
  (:returns logic-variable)
  "Return the variable part of a binding."
  (car binding))

(defun* binding-value ((binding binding))
  "Return the value part of a binding."
  (cdr binding))

(defun* lookup ((variable logic-variable)
                (bindings binding-list))
  "Return the value the given variable is bound to."
  (binding-value (get-binding variable bindings)))

(defun* extend-bindings ((variable logic-variable)
                         (value t)
                         (bindings binding-list))
  (:returns binding-list)
  "Add a binding (var . val) to the binding list (nondestructively)."
  (cons (cons variable value)
        (if (and (eq bindings no-bindings))
          nil
          bindings)))

(defun* match-variable ((variable logic-variable)
                        (input t)
                        (bindings binding-list))
  "Match the var with input, using (possibly updating) and returning bindings."
  (let ((binding (get-binding variable bindings)))
    (cond ((not binding)
           (extend-bindings variable input bindings))
          ((equal input (binding-value binding))
           bindings)
          (t fail))))

(defun* check-occurs ((variable logic-variable)
                      (target t)
                      (bindings binding-list))
  (:returns boolean)
  "Check whether the variable occurs somewhere in the target.

  Takes the bindings into account.  This is expensive.

  "
  (cond
   ;; If the target is this variable, then yep.
   ((eql variable target) t)

   ;; The empty list doesn't contain anything.
   ((null target) nil)

   ;; The the target is a (different) variable that has a binding, we need to
   ;; check if the variable occurs in its bindings.
   ((and (variable-p target)
         (get-binding target bindings))
    (check-occurs variable (lookup target bindings) bindings))

   ;; If the target is a list, check if any of the elements contain the variable.
   ((listp target)
    (or (check-occurs variable (first target) bindings)
        (check-occurs variable (rest target) bindings)))

   ;; Otherwise we're safe.
   (t nil)))

(defun unify (x y &optional (bindings no-bindings))
  "Unify the two terms and return bindings necessary to do so (or FAIL)."
  (flet ((unify-variable
          (variable target bindings)
          (cond
           ;; If we've already got a binding for this variable, we can try to
           ;; unify its value with the target.
           ((get-binding variable bindings)
            (unify (lookup variable bindings) target bindings))

           ;; If the target is ALSO a variable, and it has a binding, then we
           ;; can unify this variable with the target's value.
           ((and (variable-p target) (get-binding target bindings))
            (unify variable (lookup target bindings) bindings))

           ;; If this variable occurs in the target (including in something
           ;; in its bindings) and we're checking occurrence, bail.
           ((and *check-occurs* (check-occurs variable target bindings))
            fail)

           ;; Otherwise we can just bind this variable to the target.
           (t (extend-bindings variable target bindings)))))
    (cond
     ;; Pass failures through.
     ((eq bindings fail) fail)

     ;; Trying to unify two identical objects (constants or variables) can just
     ;; return the bindings as-is.
     ;;
     ;; ex: (unify :y :y) or (unify 'foo 'foo)
     ((eql x y) bindings)

     ;; Unifying a variable with something.
     ((variable-p x) (unify-variable x y bindings))
     ((variable-p y) (unify-variable y x bindings))

     ;; Unifying a non-variable with nil should fail, except for nil itself.
     ;; But that was handled with (eql x y).
     ((or (null x) (null y)) fail)

     ;; Unifying non-empty compound terms such as
     ;; (likes :x cats) with (likes sally :y).
     ((and (listp x) (listp y))
      (unify (rest x) (rest y) ; Unify the tails with the bindings gotten from...
             (unify (first x) (first y) bindings))) ; unifying the heads.

     ;; Otherwise we're looking at different constants, or a constant and a
     ;; compound term, so just give up.
     (t fail))))


