(in-package #:bones.wam)

;;;; Opcodes
(defun* instruction-size ((opcode opcode))
  (:returns (integer 0 3))
  "Return the size of an instruction for the given opcode.

  The size includes one word for the opcode itself and one for each argument.

  "
  (eswitch (opcode)
    (+opcode-get-structure+ 3)
    (+opcode-unify-variable+ 2)
    (+opcode-unify-value+ 2)
    (+opcode-get-variable+ 3)
    (+opcode-get-value+ 3)

    (+opcode-put-structure+ 3)
    (+opcode-set-variable+ 2)
    (+opcode-set-value+ 2)
    (+opcode-put-variable+ 3)
    (+opcode-put-value+ 3)

    (+opcode-call+ 2)
    (+opcode-proceed+ 1)))


(defun* opcode-name ((opcode opcode))
  (:returns string)
  (eswitch (opcode)
    (+opcode-get-structure+ "GET-STRUCTURE")
    (+opcode-unify-variable+ "UNIFY-VARIABLE")
    (+opcode-unify-value+ "UNIFY-VALUE")
    (+opcode-get-variable+ "GET-VARIABLE")
    (+opcode-get-value+ "GET-VALUE")

    (+opcode-put-structure+ "PUT-STRUCTURE")
    (+opcode-set-variable+ "SET-VARIABLE")
    (+opcode-set-value+ "SET-VALUE")
    (+opcode-put-variable+ "PUT-VARIABLE")
    (+opcode-put-value+ "PUT-VALUE")

    (+opcode-call+ "CALL")
    (+opcode-proceed+ "PROCEED")))

(defun* opcode-short-name ((opcode opcode))
  (:returns string)
  (eswitch (opcode)
    (+opcode-get-structure+ "GETS")
    (+opcode-unify-variable+ "UVAR")
    (+opcode-unify-value+ "UVLU")
    (+opcode-get-variable+ "GVAR")
    (+opcode-get-value+ "GVLU")

    (+opcode-put-structure+ "PUTS")
    (+opcode-set-variable+ "SVAR")
    (+opcode-set-value+ "SVLU")
    (+opcode-put-variable+ "PVAR")
    (+opcode-put-value+ "PVLU")

    (+opcode-call+ "CALL")
    (+opcode-proceed+ "PROC")))


;;;; Register Designators
;;; A register designator is a number that specifies a particular register.
;;;
;;; The register might be a local register (A_n or X_n in WAMspeak) for holding
;;; temporary things or a stack register (Y_n) for holding permanent variables.
;;;
;;; Internally register designators are implemented as a bitmasked value/tag:
;;;
;;;    value          tag bit
;;;    rrrrrrrrrrrrrrrT
;;;
;;; But you should probably just use this interface to interact with them.

(defun* register-designator-tag ((register-designator register-designator))
  (:returns register-designator-tag)
  (logand register-designator +register-designator-tag-bitmask+))

(defun* register-designator-value ((register-designator register-designator))
  (:returns register-index)
  (ash register-designator -1))


(defun* register-designator-local-p ((register-designator register-designator))
  (:returns boolean)
  (= +tag-local-register+
     (register-designator-tag register-designator)))

(defun* register-designator-stack-p ((register-designator register-designator))
  (:returns boolean)
  (= +tag-stack-register+
     (register-designator-tag register-designator)))


(defun* make-register-designator ((register register-index)
                                  (tag register-designator-tag))
  (:returns register-designator)
  (logior (ash register 1)
          tag))

(defun* make-local-register-designator ((register register-index))
  (:returns register-designator)
  (make-register-designator register +tag-local-register+))

(defun* make-stack-register-designator ((register register-index))
  (:returns register-designator)
  (make-register-designator register +tag-stack-register+))
