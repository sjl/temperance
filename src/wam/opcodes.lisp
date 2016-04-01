(in-package #:bones.wam)

;;; This file contains some basic helpers for working with opcodes themselves.
;;; For the actual implementation of the instructions, see instructions.lisp.


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
    (+opcode-put-value+ "PUT-VALUE")))

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
    (+opcode-put-value+ "PVLU")))
