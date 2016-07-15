(in-package #:bones.wam)

;;;; .-,--.
;;;;  '|__/ ,-. ,-. ,-. . ,-. ,-.
;;;;  ,|    ,-| |   `-. | | | | |
;;;;  `'    `-^ '   `-' ' ' ' `-|
;;;;                           ,|
;;;;                           `'

(defclass node () ())

(defclass top-level-node (node)
  ((functor :accessor node-functor
            :type symbol
            :initarg :functor)
   (arity :accessor node-arity
          :type arity
          :initarg :arity)
   (arguments :accessor node-arguments
              :type list
              :initarg :arguments)))

(defclass vanilla-node (node)
  ((register :accessor node-register
             :type register
             :documentation "The register allocated to store this node.")))


(defclass structure-node (vanilla-node)
  ((functor :accessor node-functor
            :type symbol
            :initarg :functor)
   (arity :accessor node-arity
          :type arity
          :initarg :arity)
   (arguments :accessor node-arguments
              :type list
              :initarg :arguments)))

(defclass variable-node (vanilla-node)
  ((variable :accessor node-variable
             :type symbol
             :initarg :variable)))

(defclass argument-variable-node (variable-node)
  ((secondary-register
     :accessor node-secondary-register
     :type register
     :documentation
     "The register that actually holds the variable (NOT the argument register).")))

(defclass list-node (vanilla-node)
  ((head :accessor node-head :type node :initarg :head)
   (tail :accessor node-tail :type node :initarg :tail)))

(defclass lisp-object-node (vanilla-node)
  ((object :accessor node-object :type t :initarg :object)))


; todo functor -> fname
(defun* make-top-level-node ((functor symbol) (arity arity) (arguments list))
  (:returns top-level-node)
  (values (make-instance 'top-level-node
                         :functor functor
                         :arity arity
                         :arguments arguments)))

(defun* make-structure-node ((functor symbol) (arity arity) (arguments list))
  (:returns structure-node)
  (values (make-instance 'structure-node
                         :functor functor
                         :arity arity
                         :arguments arguments)))

(defun* make-variable-node ((variable symbol))
  (:returns variable-node)
  (values (make-instance 'variable-node :variable variable)))

(defun* make-argument-variable-node ((variable symbol))
  (:returns variable-node)
  (values (make-instance 'argument-variable-node :variable variable)))

(defun* make-list-node ((head node) (tail node))
  (:returns list-node)
  (values (make-instance 'list-node :head head :tail tail)))

(defun* make-lisp-object-node ((object t))
  (:returns lisp-object-node)
  (values (make-instance 'lisp-object-node :object object)))


(defgeneric* node-children (node)
  (:returns list)
  "Return the children of the given node.

  Presumably these will need to be traversed when allocating registers.")

(defmethod node-children ((node vanilla-node))
  (list))

(defmethod node-children ((node top-level-node))
  (node-arguments node))

(defmethod node-children ((node structure-node))
  (node-arguments node))

(defmethod node-children ((node list-node))
  (list (node-head node) (node-tail node)))


(defun* nil-node-p ((node node))
  (:returns boolean)
  "Return whether the given node is the magic nil/0 constant."
  (and (typep node 'structure-node)
       (eql (node-functor node) nil)
       (zerop (node-arity node))))


(defparameter *dump-node-indent* 0)

(defun print-node-register (node stream &optional space-before)
  (when (slot-boundp node 'register)
    (format stream (if space-before " ~A =" "~A = ") (node-register node))))

(defun print-node-secondary-register (node stream &optional space-before)
  (when (slot-boundp node 'secondary-register)
    (format stream
            (if space-before " ~A =" "~A = ")
            (node-secondary-register node))))


(defgeneric dump-node (node))

(defmethod dump-node ((node node))
  (format t "~VAAN NODE" *dump-node-indent* ""))

(defmethod dump-node ((node variable-node))
  (format t "~VA#<VAR" *dump-node-indent* "")
  (print-node-register node t t)
  (format t " ~S>" (node-variable node)))

(defmethod dump-node ((node argument-variable-node))
  (format t "~VA#<VAR" *dump-node-indent* "")
  (print-node-register node t t)
  (print-node-secondary-register node t t)
  (format t " ~S>" (node-variable node)))

(defmethod dump-node ((node structure-node))
  (format t "~VA#<STRUCT " *dump-node-indent* "")
  (print-node-register node t)
  (format t "~A/~D" (node-functor node) (node-arity node))
  (let ((*dump-node-indent* (+ *dump-node-indent* 4)))
    (dolist (a (node-arguments node))
      (terpri)
      (dump-node a)))
  (format t ">"))

(defmethod dump-node ((node list-node))
  (format t "~VA#<LIST" *dump-node-indent* "")
  (print-node-register node t t)
  (let ((*dump-node-indent* (+ *dump-node-indent* 4)))
    (loop :for element = node :then tail
          :while (typep element 'list-node)
          :for head = (node-head element)
          :for tail = (node-tail element)
          :do (progn (terpri) (dump-node head))
          :finally (when (not (nil-node-p element))
                     (format t "~%~VA.~%" *dump-node-indent* "")
                     (dump-node element))))
  (format t ">"))

(defmethod dump-node ((node lisp-object-node))
  (format t "~VA#<LISP OBJECT " *dump-node-indent* "")
  (print-node-register node t)
  (format t "~A>" (lisp-object-to-string (node-object node))))

(defmethod dump-node ((node top-level-node))
  (format t "#<~A/~D" (node-functor node) (node-arity node))
  (let ((*dump-node-indent* 4))
    (dolist (n (node-arguments node))
      (terpri)
      (dump-node n)))
  (format t ">"))

(defmethod print-object ((node node) stream)
  (let ((*standard-output* stream))
    (dump-node node)))


(defun* parse-list ((contents list))
  (:returns node)
  (if contents
    (make-list-node (parse (car contents))
                    (parse-list (cdr contents)))
    (make-structure-node 'nil 0 ())))

(defun* parse-list* ((contents list))
  (:returns node)
  (destructuring-bind (next . remaining) contents
    (if (null remaining)
      (parse next)
      (make-list-node (parse next)
                      (parse-list* remaining)))))

(defun* parse (term &optional top-level-argument)
  (:returns node)
  (cond
    ((variablep term)
     (if top-level-argument
       (make-argument-variable-node term)
       (make-variable-node term)))
    ((symbolp term)
     (parse (list term))) ; c/0 -> (c/0)
    ((consp term)
     (destructuring-bind (functor . arguments) term
       (when (not (symbolp functor))
         (error
           "Cannot parse term ~S because ~S is not a valid functor."
           term functor))
       (case functor
         (list (parse-list arguments))
         (list* (parse-list* arguments))
         (t (make-structure-node functor
                                 (length arguments)
                                 (mapcar #'parse arguments))))))
    ((numberp term)
     (make-lisp-object-node term))
    (t (error "Cannot parse term ~S into a Prolog term." term))))

(defun* parse-top-level (term)
  (:returns top-level-node)
  (typecase term
    (symbol (parse-top-level (list term))) ; c/0 -> (c/0)
    (cons (destructuring-bind (functor . arguments) term
            (when (not (symbolp functor))
              (error
                "Cannot parse top-level term ~S because ~S is not a valid functor."
                term functor))
            (make-top-level-node functor (length arguments)
                                 (mapcar (lambda (a) (parse a t))
                                         arguments))))
    (t (error "Cannot parse top-level term ~S into a Prolog term." term))))


