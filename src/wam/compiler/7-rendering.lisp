(in-package #:bones.wam)

;;;; .-,--.           .
;;;;  `|__/ ,-. ,-. ,-| ,-. ,-. . ,-. ,-.
;;;;  )| \  |-' | | | | |-' |   | | | | |
;;;;  `'  ` `-' ' ' `-^ `-' '   ' ' ' `-|
;;;;                                   ,|
;;;;                                   `'

;;; Rendering is the act of taking the friendly list-of-instructions format and
;;; actually converting it to raw-ass bytes and storing it in an array.


(defun check-instruction (opcode arguments)
  (assert (= (length arguments)
             (1- (instruction-size opcode)))
      ()
    "Cannot push opcode ~A with ~D arguments ~S, it requires exactly ~D."
    (opcode-name opcode)
    (length arguments)
    arguments
    (1- (instruction-size opcode))))


(defun* code-push-instruction ((store generic-code-store)
                               (opcode opcode)
                               (arguments list)
                               (address code-index))
  "Push the given instruction into `store` at `address`.

  `arguments` should be a list of `code-word`s.

  Returns how many words were pushed.

  "
  (check-instruction opcode arguments)
  (setf (aref store address) opcode
        (subseq store (1+ address)) arguments)
  (instruction-size opcode))


(defun* render-opcode ((opcode-designator keyword))
  (ecase opcode-designator
    (:get-structure          +opcode-get-structure+)
    (:get-variable-local     +opcode-get-variable-local+)
    (:get-variable-stack     +opcode-get-variable-stack+)
    (:get-value-local        +opcode-get-value-local+)
    (:get-value-stack        +opcode-get-value-stack+)
    (:put-structure          +opcode-put-structure+)
    (:put-variable-local     +opcode-put-variable-local+)
    (:put-variable-stack     +opcode-put-variable-stack+)
    (:put-value-local        +opcode-put-value-local+)
    (:put-value-stack        +opcode-put-value-stack+)
    (:subterm-variable-local +opcode-subterm-variable-local+)
    (:subterm-variable-stack +opcode-subterm-variable-stack+)
    (:subterm-value-local    +opcode-subterm-value-local+)
    (:subterm-value-stack    +opcode-subterm-value-stack+)
    (:subterm-void           +opcode-subterm-void+)
    (:put-constant           +opcode-put-constant+)
    (:get-constant           +opcode-get-constant+)
    (:subterm-constant       +opcode-subterm-constant+)
    (:get-list               +opcode-get-list+)
    (:put-list               +opcode-put-list+)
    (:get-lisp-object        +opcode-get-lisp-object+)
    (:put-lisp-object        +opcode-put-lisp-object+)
    (:jump                   +opcode-jump+)
    (:call                   +opcode-call+)
    (:dynamic-jump           +opcode-dynamic-jump+)
    (:dynamic-call           +opcode-dynamic-call+)
    (:proceed                +opcode-proceed+)
    (:allocate               +opcode-allocate+)
    (:deallocate             +opcode-deallocate+)
    (:done                   +opcode-done+)
    (:try                    +opcode-try+)
    (:retry                  +opcode-retry+)
    (:trust                  +opcode-trust+)
    (:cut                    +opcode-cut+)))

(defun* render-argument (argument)
  (cond
    ;; Ugly choice point args that'll be filled later...
    ((eq +choice-point-placeholder+ argument) 0)

    ;; Bytecode just needs the register numbers.
    ((typep argument 'register) (register-number argument))

    ;; Everything else just gets shoved right into the array.
    (t argument)))

(defun* render-bytecode ((store generic-code-store)
                         (instructions circle)
                         (start code-index)
                         (limit code-index))
  "Render `instructions` (a circle) into `store` starting at `start`.

  Bail if ever pushed beyond `limit`.

  Return the total number of code words rendered.

  "
  (let ((previous-jump nil))
    (flet
        ((fill-previous-jump (address)
           (when previous-jump
             (setf (aref store (1+ previous-jump)) address))
           (setf previous-jump address)))
      (loop
        :with address = start

        ;; Render the next instruction
        :for (opcode-designator . arguments) :in (circle-to-list instructions)
        :for opcode = (render-opcode opcode-designator)
        :for size = (instruction-size opcode)
        :summing size

        ;; Make sure we don't run past the end of our section.
        :when (>= (+ size address) limit)
        :do (error "Code store exhausted, game over.")

        :do (code-push-instruction store
                                   opcode
                                   (mapcar #'render-argument arguments)
                                   address)

        ;; We need to fill in the addresses for the choice point jumping
        ;; instructions.  For example, when we have TRY ... TRUST, the TRUST
        ;; needs to patch its address into the TRY instruction.
        ;;
        ;; I know, this is ugly, sorry.
        :when (member opcode-designator '(:try :retry :trust))
        :do (fill-previous-jump address)

        ;; look, don't judge me, i told you i know its bad
        :do (incf address size)))))


(defun* render-query ((wam wam) (instructions circle))
  (render-bytecode (wam-code wam) instructions 0 +maximum-query-size+))


(defun* mark-label ((wam wam)
                    (functor symbol)
                    (arity arity)
                    (address code-index))
  "Set the code label `functor`/`arity` to point at `address`."
  (setf (wam-code-label wam functor arity)
        address))

(defun* render-rules ((wam wam)
                      (functor symbol)
                      (arity arity)
                      (instructions circle))
  ;; Before we render the instructions, make the label point at where they're
  ;; about to go.
  (mark-label wam functor arity (wam-code-pointer wam))
  (incf (wam-code-pointer wam)
        (render-bytecode (wam-code wam)
                         instructions
                         (wam-code-pointer wam)
                         (array-total-size (wam-code wam)))))



