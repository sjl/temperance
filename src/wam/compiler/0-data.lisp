(in-package #:bones.wam)

;;;; .-,--.      .
;;;; ' |   \ ,-. |- ,-.
;;;; , |   / ,-| |  ,-|
;;;; `-^--'  `-^ `' `-^

;;;; Constants
(defconstant +choice-point-placeholder+ 'choice-point-placeholder)


;;;; Utils
(declaim (inline variablep))

(defun* variablep (term)
  (and (symbolp term)
       (char= (char (symbol-name term) 0) #\?)))

(defun lisp-object-to-string (o)
  (with-output-to-string (str)
    (print-unreadable-object (o str :type t :identity t))))

(defun required ()
  (error "Argument required."))


;;;; Registers
(declaim (inline register-type register-number make-register register=
                 register-argument-p
                 register-temporary-p
                 register-permanent-p
                 register-anonymous-p))


(deftype register-type ()
  '(member :argument :local :permanent :anonymous))

(deftype register-number ()
  `(integer 0 ,(1- +register-count+)))


(defstruct (register (:constructor make-register (type number)))
  (type (required) :type register-type)
  (number (required) :type register-number))


(defun* make-temporary-register ((number register-number) (arity arity))
  (make-register (if (< number arity) :argument :local)
                 number))

(defun* make-permanent-register ((number register-number))
  (make-register :permanent number))

(defun* make-anonymous-register ()
  (make-register :anonymous 0))


(defun* register-to-string ((register register))
  (if (eq (register-type register) :anonymous)
    "__"
    (format nil "~A~D"
            (ecase (register-type register)
              (:argument #\A)
              (:local #\X)
              (:permanent #\Y))
            (+ (register-number register)
               (if *off-by-one* 1 0)))))

(defmethod print-object ((object register) stream)
  (print-unreadable-object (object stream :identity nil :type nil)
    (format stream (register-to-string object))))


(defun* register-argument-p ((register register))
  (eq (register-type register) :argument))

(defun* register-temporary-p ((register register))
  (and (member (register-type register) '(:argument :local)) t))

(defun* register-permanent-p ((register register))
  (eq (register-type register) :permanent))

(defun* register-anonymous-p ((register register))
  (eq (register-type register) :anonymous))


(defun* register= ((r1 register) (r2 register))
  (and (eq (register-type r1)
           (register-type r2))
       (= (register-number r1)
          (register-number r2))))



;;;; Clause Properties
;;; When tokenizing/precompiling a clause there are a few pieces of metadata
;;; we're going to need.  We group them into a struct to make it easier to pass
;;; everything around.

(defstruct (clause-properties (:conc-name clause-))
  (nead-vars nil :type list)
  (nead-arity 0 :type arity)
  (permanent-vars nil :type list)
  (anonymous-vars nil :type list))


(defun* find-variables ((terms list))
  "Return the set of variables in `terms`."
  (let ((variables nil))
    (recursively ((term terms))
      (cond
        ((variablep term) (pushnew term variables))
        ((consp term) (recur (car term))
                      (recur (cdr term)))
        (t nil)))
    variables))

(defun find-shared-variables (terms)
  "Return the set of all variables shared by two or more terms."
  (labels
      ((count-uses (variable)
         (count-if (curry #'tree-member-p variable) terms))
       (shared-p (variable)
         (> (count-uses variable) 1)))
    (remove-if-not #'shared-p (find-variables terms))))

(defun find-permanent-variables (clause)
  "Return a list of all the permanent variables in `clause`.

  Permanent variables are those that appear in more than one goal of the clause,
  where the head of the clause is considered to be a part of the first goal.

  "
  (if (<= (length clause) 2)
    (list) ; Facts and chain rules have no permanent variables at all
    (destructuring-bind (head body-first . body-rest) clause
      ;; The head is treated as part of the first goal for the purposes of
      ;; finding permanent variables.
      (find-shared-variables (cons (cons head body-first) body-rest)))))

(defun find-nead-variables (clause)
  "Return a list of all variables in the nead of `clause`.

  The head and neck (first term in the body) are the 'nead'.

  "
  (if (<= (length clause) 1)
    (list)
    (destructuring-bind (head body-first . body-rest) clause
      (declare (ignore body-rest))
      (find-variables (list head body-first)))))

(defun find-anonymous-variables (clause)
  "Return a list of all anonymous variables in `clause`.

  Anonymous variables are variables that are only ever used once.

  "
  (let ((seen nil)
        (once nil))
    (recursively ((term clause))
      (cond
        ((variablep term)
         (if (member term seen)
           (when (member term once)
             (setf once (delete term once)))
           (progn (push term seen)
                  (push term once))))
        ((consp term) (recur (car term))
                      (recur (cdr term)))
        (t nil)))
    once))


(defun* determine-clause-properties (head body)
  (let* ((clause
           (cons head body))
         (permanent-vars
           (if (null head)
             ;; For query clauses we cheat a bit and make ALL variables
             ;; permanent, so we can extract their bindings as results later.
             (find-variables body)
             (find-permanent-variables clause)))
         (anonymous-vars
           (if (null head)
             ;; Again, for queries we cheat and never let anything be
             ;; anonymous (except for the wildcard).
             (list +wildcard-symbol+)
             (cons +wildcard-symbol+
                   (find-anonymous-variables clause))))
         (nead-vars
           (set-difference (find-nead-variables clause)
                           permanent-vars))
         (nead-arity
           (max (1- (length head))
                (1- (length (first (remove '! body))))))) ; gross
    (make-clause-properties :nead-vars nead-vars
                            :nead-arity nead-arity
                            :permanent-vars permanent-vars
                            :anonymous-vars anonymous-vars)))


