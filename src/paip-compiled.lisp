(in-package #:bones.paip)

;;;; Variables
(define-constant unbound "Unbound"
  :test #'equal
  :documentation "A magic constant representing an unbound variable.")

(defvar *var-counter* 0
  "The number of variables created so far.")

(defstruct (var (:constructor ? ())
                (:print-function print-var))
  (name (incf *var-counter*)) ; The variable's name (defaults to a new number)
  (binding unbound)) ; The variable's binding (defaults to unbound)

(defun* print-var ((var var) stream depth)
  (if (or (and (numberp *print-level*)
               (>= depth *print-level*))
          (var-p (deref var)))
    (format stream "?~A" (var-name var))
    (write var :stream stream)))

(defun* bound-p ((var var))
  (:returns boolean)
  "Return whether the given variable has been bound."
  (not (eq (var-binding var) unbound)))

(defmacro deref (expr)
  "Chase all the bindings for the given expression in place."
  `(progn
    (loop :while (and (var-p ,expr) (bound-p ,expr))
          :do (setf ,expr (var-binding ,expr)))
    ,expr))


;;;; Bindings
(defvar *trail* (make-array 200 :fill-pointer 0 :adjustable t)
  "The trail of variable bindings performed so far.")

(defun* set-binding! ((var var) value)
  (:returns (eql t))
  "Set `var`'s binding to `value` after saving it in the trail.

  Always returns `t` (success).

  "
  (when (not (eq var value))
    (vector-push-extend var *trail*)
    (setf (var-binding var) value))
  t)

(defun* undo-bindings! ((old-trail integer))
  (:returns :void)
  "Undo all bindings back to a given point in the trail.

  The point is specified by giving the desired fill pointer.

  "
  (loop :until (= (fill-pointer *trail*) old-trail)
        :do (setf (var-binding (vector-pop *trail*)) unbound))
  (values))


;;;; Unification
(defun* unify! (x y)
  (:returns boolean)
  "Destructively unify two expressions, returning whether it was successful.

  Any variables in `x` and `y` may have their bindings set.

  "
  (cond
    ;; If they're identical objects (taking bindings into account), they unify.
    ((eql (deref x) (deref y)) t)

    ;; If they're not identical, but one is a variable, bind it to the other.
    ((var-p x) (set-binding! x y))
    ((var-p y) (set-binding! y x))

    ;; If they're both non-empty lists, unify the cars and cdrs.
    ((and (consp x) (consp y))
     (and (unify! (first x) (first y))
          (unify! (rest x) (rest y))))

    ;; Otherwise they don't unify.
    (t nil)))
