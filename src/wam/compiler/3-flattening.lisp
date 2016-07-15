(in-package #:bones.wam)

;;;; .-,--' .      .  .
;;;;  \|__  |  ,-. |- |- ,-. ,-. . ,-. ,-.
;;;;   |    |  ,-| |  |  |-' | | | | | | |
;;;;  `'    `' `-^ `' `' `-' ' ' ' ' ' `-|
;;;;                                    ,|
;;;;                                    `'

;;; "Flattening" is the process of turning a parse tree (with register
;;; assignments) into a flat list of nodes, which will then be turned into
;;; a series of instructions.
;;;
;;; The order of this list depends on whether we're compiling a query term or
;;; a program term.
;;;
;;; Turns:
;;;
;;;   X0 <- p(X1, X2)
;;;   X1 <- A
;;;   X2 <- q(X1, X3)
;;;   X3 <- B
;;;
;;; into something like:
;;;
;;;   X2 <- q(X1, X3)
;;;   X0 <- p(X1, X2)


(defclass register-assignment ()
  ((register :accessor assignment-register :type register :initarg :register)))


(defclass structure-assignment (register-assignment)
  ((functor :accessor assignment-functor :type symbol :initarg :functor)
   (arity :accessor assignment-arity :type arity :initarg :arity)
   (arguments :accessor assignment-arguments :type list :initarg :arguments)))

(defclass argument-variable-assignment (register-assignment)
  ((target :accessor assignment-target :type register :initarg :target)))

(defclass list-assignment (register-assignment)
  ((head :accessor assignment-head :type register :initarg :head)
   (tail :accessor assignment-tail :type register :initarg :tail)))

(defclass lisp-object-assignment (register-assignment)
  ((object :accessor assignment-object :type t :initarg :object)))


(defmethod print-object ((assignment structure-assignment) stream)
  (print-unreadable-object (assignment stream :type nil :identity nil)
    (format stream "~A = ~A/~D(~{~A~^, ~})"
            (register-to-string (assignment-register assignment))
            (assignment-functor assignment)
            (assignment-arity assignment)
            (mapcar #'register-to-string (assignment-arguments assignment)))))

(defmethod print-object ((assignment argument-variable-assignment) stream)
  (print-unreadable-object (assignment stream :type nil :identity nil)
    (format stream "~A = ~A"
            (register-to-string (assignment-register assignment))
            (register-to-string (assignment-target assignment)))))

(defmethod print-object ((assignment list-assignment) stream)
  (print-unreadable-object (assignment stream :type nil :identity nil)
    (format stream "~A = [~A | ~A]"
            (register-to-string (assignment-register assignment))
            (register-to-string (assignment-head assignment))
            (register-to-string (assignment-tail assignment)))))

(defmethod print-object ((assignment lisp-object-assignment) stream)
  (print-unreadable-object (assignment stream :type nil :identity nil)
    (format stream "~A = ~A"
            (register-to-string (assignment-register assignment))
            (lisp-object-to-string (assignment-object assignment)))))


(defgeneric* node-flatten (node)
  (:returns (or null register-assignment)))

(defmethod node-flatten (node)
  nil)

(defmethod node-flatten ((node structure-node))
  (values (make-instance 'structure-assignment
                         :register (node-register node)
                         :functor (node-functor node)
                         :arity (node-arity node)
                         :arguments (mapcar #'node-register (node-arguments node)))))

(defmethod node-flatten ((node argument-variable-node))
  (values (make-instance 'argument-variable-assignment
                         :register (node-register node)
                         :target (node-secondary-register node))))

(defmethod node-flatten ((node list-node))
  (values (make-instance 'list-assignment
                         :register (node-register node)
                         :head (node-register (node-head node))
                         :tail (node-register (node-tail node)))))

(defmethod node-flatten ((node lisp-object-node))
  (values (make-instance 'lisp-object-assignment
                         :register (node-register node)
                         :object (node-object node))))


(defun* flatten-breadth-first ((tree top-level-node))
  (:returns list)
  (let ((results nil))
    (recursively ((node tree))
      (when-let (assignment (node-flatten node))
        (push assignment results))
      (mapcar #'recur (node-children node)))
    (nreverse results)))

(defun* flatten-depth-first-post-order ((tree top-level-node))
  (:returns list)
  (let ((results nil))
    (recursively ((node tree))
      (mapcar #'recur (node-children node))
      (when-let (assignment (node-flatten node))
        (push assignment results)))
    (nreverse results)))


(defun* flatten-query ((tree top-level-node))
  (:returns list)
  (flatten-depth-first-post-order tree))

(defun* flatten-program ((tree top-level-node))
  (:returns list)
  (flatten-breadth-first tree))


