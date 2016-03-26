(in-package #:bones.paip)

;;;; Types
(deftype logic-variable ()
  'symbol)

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
  (and (symbolp term)
       (equal (char (symbol-name term) 0)
              #\?)))


(defun* get-binding ((variable logic-variable)
                     (bindings binding-list))
  (:returns (or binding null))
  "Return the binding (var . val) for the given variable, or nil."
  (assoc variable bindings))

(defun* has-binding ((variable logic-variable)
                     (bindings binding-list))
  (:returns boolean)
  (not (null (get-binding variable bindings))))


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
        (if (and (equal bindings no-bindings))
          nil
          bindings)))


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
  (flet ((unify-variable (variable target bindings)
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


;;;; Substitution
(defun* substitute-bindings ((bindings binding-list)
                             (form t))
  "Substitute (recursively) the bindings into the given form."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) form)
        ((and (variable-p form) (get-binding form bindings))
         (substitute-bindings bindings
                              (lookup form bindings)))
        ((atom form) form)
        (t (mapcar (curry #'substitute-bindings bindings) form))))

(defun unifier (x y)
  "Unify x with y and substitute in the bindings to get the result."
  (substitute-bindings (unify x y) x))


;;;; Database
;;; A clause is an assertion in the database.  There are two types.
;;;
;;; A fact is the "base" clause:
;;;
;;;   (likes kim cats)
;;;
;;; A rule is a way to deduce new facts from existing information:
;;;
;;;   ((likes sally :x)
;;;    (likes :x cats))
;;;
;;; Clauses are stored as lists.  The head is the first item in the list, and
;;; it's "the thing you're trying to prove".  You prove it by proving all the
;;; things in the tail of the list.  For facts the tail is empty, so they are
;;; trivially proven.
;;;
;;; A predicate is the named head of a part of a clause.  In `(likes sally :x)`
;;; the predicate is `likes`.
;;;
;;; Predicates are stored in the plists of their symbols, which is a little
;;; insane, but it's how Norvig did it so I'll do it like this for now.
(defvar *db-predicates* nil
  "A list of all the predicates in the database.")

(defconstant clause-key 'bones.paip-clauses
  "The key to use in the symbol plist for the clauses.")

(defun clause-head (clause)
  (first clause))

(defun clause-body (clause)
  (rest clause))


(defun get-clauses (pred)
  (get pred clause-key))

(defun set-clauses (pred clauses)
  (setf (get pred clause-key) clauses))


(defun predicate (relation)
  (first relation))


(defun wildcard-variable-p (form)
  (and (symbolp form) (string= (symbol-name form) "?")))

(defun replace-wildcard-variables (form)
  (cond
   ((wildcard-variable-p form) (gensym "?"))
   ((atom form) form)
   ((consp form) (cons (replace-wildcard-variables (car form))
                       (replace-wildcard-variables (cdr form))))
   (t (mapcar #'replace-wildcard-variables form))))


(defun add-clause (clause)
  (let ((pred (predicate (clause-head clause))))
    (assert (and (symbolp pred)
                 (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (set-clauses pred
                 (nconc (get-clauses pred) (list clause)))
    pred))


(defmacro rule (&rest clause)
  `(add-clause ',(replace-wildcard-variables clause)))

(defmacro fact (&rest body)
  `(add-clause '(,(replace-wildcard-variables body))))


(defun clear-predicate (predicate)
  (setf (get predicate clause-key) nil))

(defun clear-db ()
  (mapc #'clear-predicate *db-predicates*))


;;;; Proving
(defun unique-find-anywhere-if (predicate tree &optional acc)
  (if (atom tree)
    (if (funcall predicate tree)
      (adjoin tree acc)
      acc)
    (unique-find-anywhere-if predicate
                             (first tree)
                             (unique-find-anywhere-if predicate (rest tree) acc))))

(defun variables-in (expr)
  (unique-find-anywhere-if #'variable-p expr))

(defun rename-variables (form)
  "Replace all variables in the form with new (unique) ones."
  (sublis (mapcar #'(lambda (variable)
                      (cons variable (gensym (string variable))))
                  (variables-in form))
          form))


(defun prove-clause (goal bindings other-goals clause)
  (let ((new-clause (rename-variables clause)))
    (prove-all (append (clause-body new-clause) other-goals)
               (unify goal (clause-head new-clause) bindings))))

(defun prove (goal bindings other-goals)
  "Return a list of possible solutions to the given goal."
  (let ((clauses (get-clauses (predicate goal))))
    (if (listp clauses)
      (some (curry #'prove-clause goal bindings other-goals) clauses)
      (funcall clauses (rest goal) bindings other-goals))))

(defun prove-all (goals bindings)
  "Return a solution to the conjunction of goals."
  (cond
   ;; If something failed further up the pipeline, bail here.
   ((eq bindings fail) fail)

   ;; If there's nothing to prove, it's vacuously true.  Return the bindings as
   ;; the result.
   ((null goals) bindings)

   (t (prove (first goals) bindings (rest goals)))))


;;;; Cleaning Solutions
(defun clean-variables (variables solution)
  (mapcar #'(lambda (var)
             (cons var (substitute-bindings solution var)))
    variables))


;;;; Querying Interface
(defun continue-never ()
  nil)

(defun continue-always ()
  t)

(defun continue-ask ()
  (case (read-char)
    (#\; t)
    (#\. nil)
    (#\newline (continue-ask))
    (otherwise
      (format t " Type ; to see more or . to stop")
      (continue-ask))))


(defun show-prolog-vars (variables bindings other-goals continue-p)
  (if (null variables)
    (format t "~&Yes")
    (dolist (var variables)
      (format t "~&~A = ~A"
              var (substitute-bindings bindings var))))
  (finish-output)
  (if (funcall continue-p)
    fail
    (prove-all other-goals bindings)))


(defun show-prolog-vars-ask (variables bindings other-goals)
  (show-prolog-vars variables bindings other-goals #'continue-ask))

(defun show-prolog-vars-all (variables bindings other-goals)
  (show-prolog-vars variables bindings other-goals #'continue-always))

(defun show-prolog-vars-one (variables bindings other-goals)
  (show-prolog-vars variables bindings other-goals #'continue-never))

(setf (get 'show-prolog-vars-ask clause-key) 'show-prolog-vars-ask)
(setf (get 'show-prolog-vars-all clause-key) 'show-prolog-vars-all)
(setf (get 'show-prolog-vars-one clause-key) 'show-prolog-vars-one)


(defun top-level-query (goals primitive)
  (prove-all `(,@(replace-wildcard-variables goals)
               (,primitive
                ,@(remove-if #'wildcard-variable-p (variables-in goals))))
             no-bindings)
  (format t "~&No."))


(defmacro query (&rest goals)
  "Perform the query interactively."
  `(top-level-query ',goals 'show-prolog-vars-ask))

(defmacro query-all (&rest goals)
  "Perform the query and automatically show all results."
  `(top-level-query ',goals 'show-prolog-vars-all))

(defmacro query-one (&rest goals)
  "Perform the query and just show the first result."
  `(top-level-query ',goals 'show-prolog-vars-one))


;;;; Finding Interface
(defparameter *results* nil)


(defun return-one-result (variables bindings other-goals)
  (setf *results* (clean-variables variables bindings))
  (prove-all other-goals bindings))

(defun return-all-results (variables bindings other-goals)
  (declare (ignore other-goals))
  (push (clean-variables variables bindings) *results*)
  fail)

(setf (get 'return-one-result clause-key) 'return-one-result)
(setf (get 'return-all-results clause-key) 'return-all-results)


(defun top-level-find (goals primitive)
  (let ((*results* (list)))
    (prove-all `(,@(replace-wildcard-variables goals)
                 (,primitive
                  ,@(remove-if #'wildcard-variable-p (variables-in goals))))
               no-bindings)
    *results*))


(defmacro return-one (&rest goals)
  `(top-level-find ',goals 'return-one-result))

(defmacro return-all (&rest goals)
  `(top-level-find ',goals 'return-all-results))

