(in-package #:bones.wam)

;;;; .-,--.
;;;;  '|__/ ,-. ,-. ,-. . ,-. ,-.
;;;;  ,|    ,-| |   `-. | | | | |
;;;;  `'    `-^ '   `-' ' ' ' `-|
;;;;                           ,|
;;;;                           `'

; todo functor -> fname

(defstruct node)


(defstruct (top-level-node (:include node))
  (functor nil :type symbol)
  (arity 0 :type arity)
  (arguments nil :type list))

(defstruct (vanilla-node (:include node)
                         (:conc-name node-))
  ;; The register allocated to store this node.
  (register nil :type (or null register)))


(defstruct (structure-node (:include vanilla-node)
                           (:conc-name node-))
  (functor nil :type symbol)
  (arity 0 :type arity)
  (arguments nil :type list))

(defstruct (variable-node (:include vanilla-node)
                          (:conc-name node-))
  (variable nil :type symbol))

(defstruct (argument-variable-node (:include variable-node)
                                   (:conc-name node-))
  ;; The register that actually holds the variable (NOT the argument register).
  (secondary-register nil :type (or null register)))

(defstruct (list-node (:include vanilla-node)
                      (:conc-name node-))
  (head (error "Head argument required") :type node)
  (tail (error "Head argument required") :type node))

(defstruct (lisp-object-node (:include vanilla-node)
                             (:conc-name node-))
  (object nil :type t))


(defgeneric* node-children (node)
  "Return the children of the given node.

  Presumably these will need to be traversed when allocating registers.")

(defmethod node-children ((node vanilla-node))
  (list))

(defmethod node-children ((node top-level-node))
  (top-level-node-arguments node))

(defmethod node-children ((node structure-node))
  (node-arguments node))

(defmethod node-children ((node list-node))
  (list (node-head node) (node-tail node)))


(defun* nil-node-p ((node node))
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
  (with-slots (functor arity arguments) node
    (format t "#<~A/~D" functor arity)
    (let ((*dump-node-indent* 4))
      (dolist (n arguments)
        (terpri)
        (dump-node n)))
    (format t ">")))

(defmethod print-object ((node node) stream)
  (let ((*standard-output* stream))
    (dump-node node)))


(defun* parse-list ((contents list))
  (if contents
    (make-list-node :head (parse (car contents))
                    :tail (parse-list (cdr contents)))
    (make-structure-node :functor nil
                         :arity 0
                         :arguments ())))

(defun* parse-list* ((contents list))
  (destructuring-bind (next . remaining) contents
    (if (null remaining)
      (parse next)
      (make-list-node :head (parse next)
                      :tail (parse-list* remaining)))))

(defun* parse (term &optional top-level-argument)
  (cond
    ((variablep term)
     (if top-level-argument
       (make-argument-variable-node :variable term)
       (make-variable-node :variable term)))
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
         (t (make-structure-node :functor functor
                                 :arity (length arguments)
                                 :arguments (mapcar #'parse arguments))))))
    ((numberp term)
     (make-lisp-object-node :object term))
    (t (error "Cannot parse term ~S into a Prolog term." term))))

(defun* parse-top-level (term)
  (typecase term
    (symbol (parse-top-level (list term))) ; c/0 -> (c/0)
    (cons (destructuring-bind (functor . arguments) term
            (when (not (symbolp functor))
              (error
                "Cannot parse top-level term ~S because ~S is not a valid functor."
                term functor))
            (make-top-level-node :functor functor
                                 :arity (length arguments)
                                 :arguments (mapcar (lambda (a) (parse a t))
                                                    arguments))))
    (t (error "Cannot parse top-level term ~S into a Prolog term." term))))


