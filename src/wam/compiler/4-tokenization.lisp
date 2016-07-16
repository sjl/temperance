(in-package #:bones.wam)

;;;; ,--,--'  .                     .
;;;; `- | ,-. | , ,-. ,-. . ,_, ,-. |- . ,-. ,-.
;;;;  , | | | |<  |-' | | |  /  ,-| |  | | | | |
;;;;  `-' `-' ' ` `-' ' ' ' '"' `-^ `' ' `-' ' '

;;; Tokenizing takes a flattened set of assignments and turns it into a stream
;;; of structure assignments and bare registers.
;;;
;;; It turns:
;;;
;;;   X2 <- q(X1, X3)
;;;   X0 <- p(X1, X2)
;;;   A3 <- X4
;;;
;;; into something like:
;;;
;;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2, (A3 = X4)


(defclass token () ())


(defclass register-token (token)
  ((register :accessor token-register :type register :initarg :register)))

(defclass structure-token (register-token)
  ((functor :accessor token-functor :type symbol :initarg :functor)
   (arity :accessor token-arity :type arity :initarg :arity)))

(defclass argument-variable-token (register-token)
  ((target :accessor token-target :type register :initarg :target)))

(defclass list-token (register-token) ())

(defclass lisp-object-token (register-token)
  ((object :accessor token-object :type t :initarg :object)))

(defclass procedure-call-token ()
  ((functor :accessor token-functor :type symbol :initarg :functor)
   (arity :accessor token-arity :type arity :initarg :arity)))

(defclass call-token (procedure-call-token) ())

(defclass jump-token (procedure-call-token) ())

(defclass cut-token (token) ())


(defun make-register-token (register)
  (values (make-instance 'register-token :register register)))


(defmethod print-object ((token register-token) stream)
  (print-object (token-register token) stream))

(defmethod print-object ((token structure-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "~A = ~A/~D"
            (register-to-string (token-register token))
            (token-functor token)
            (token-arity token))))

(defmethod print-object ((token argument-variable-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "~A = ~A"
            (register-to-string (token-register token))
            (register-to-string (token-target token)))))

(defmethod print-object ((token list-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "~A = LIST" (register-to-string (token-register token)))))

(defmethod print-object ((token lisp-object-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "~A = ~A"
            (register-to-string (token-register token))
            (lisp-object-to-string (token-object token)))))

(defmethod print-object ((token call-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "CALL ~A/~D"
            (token-functor token)
            (token-arity token))))

(defmethod print-object ((token jump-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "JUMP ~A/~D"
            (token-functor token)
            (token-arity token))))

(defmethod print-object ((token cut-token) stream)
  (print-unreadable-object (token stream :identity nil :type nil)
    (format stream "CUT!")))


(defgeneric tokenize-assignment (assignment)
  (:documentation "Tokenize `assignment` into a flat list of tokens."))

(defmethod tokenize-assignment ((assignment structure-assignment))
  (list* (make-instance 'structure-token
                        :register (assignment-register assignment)
                        :functor (assignment-functor assignment)
                        :arity (assignment-arity assignment))
         (mapcar #'make-register-token (assignment-arguments assignment))))

(defmethod tokenize-assignment ((assignment argument-variable-assignment))
  (list (make-instance 'argument-variable-token
                       :register (assignment-register assignment)
                       :target (assignment-target assignment))))

(defmethod tokenize-assignment ((assignment list-assignment))
  (list (make-instance 'list-token :register (assignment-register assignment))
        (make-register-token (assignment-head assignment))
        (make-register-token (assignment-tail assignment))))

(defmethod tokenize-assignment ((assignment lisp-object-assignment))
  (list (make-instance 'lisp-object-token
                       :register (assignment-register assignment)
                       :object (assignment-object assignment))))

(defun tokenize-assignments (assignments)
  "Tokenize a flattened set of register assignments into a stream."
  (mapcan #'tokenize-assignment assignments))


(defun tokenize-program-term (term clause-props)
  "Tokenize `term` as a program term, returning its tokens."
  (let ((tree (parse-top-level term)))
    (allocate-registers tree clause-props :nead t)
    (-> tree flatten-program tokenize-assignments)))

(defun tokenize-query-term (term clause-props &key in-nead is-tail)
  "Tokenize `term` as a query term, returning its tokens."
  (let ((tree (parse-top-level term)))
    (allocate-registers tree clause-props :nead in-nead)
    (-<> tree
      flatten-query
      tokenize-assignments
      ;; We need to shove a CALL/JUMP token onto the end.
      (append <> (list (make-instance
                         (if is-tail 'jump-token 'call-token)
                         :functor (top-level-node-functor tree)
                         :arity (top-level-node-arity tree)))))))



