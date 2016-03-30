(in-package #:bones.wam)

;;; This file contains some basic helpers for working with opcodes themselves.
;;; For the actual implementation of the instructions, see instructions.lisp.


(defun* instruction-size ((opcode opcode))
  (:returns (integer 0 4))
  "Return the size of an instruction for the given opcode.

  The size includes one word for the opcode itself and one for each argument.

  "
  (eswitch (opcode)
    (+opcode-get-structure+ 4)
    (+opcode-unify-variable+ 2)
    (+opcode-unify-value+ 2)))


(defun* opcode-name ((opcode opcode))
  (:returns string)
  (eswitch (opcode)
    (+opcode-get-structure+ "GET-STRUCTURE")
    (+opcode-unify-variable+ "UNIFY-VARIABLE")
    (+opcode-unify-value+ "UNIFY-VALUE")))

(defun* opcode-short-name ((opcode opcode))
  (:returns string)
  (eswitch (opcode)
    (+opcode-get-structure+ "GETS")
    (+opcode-unify-variable+ "UVAR")
    (+opcode-unify-value+ "UVLU")))
