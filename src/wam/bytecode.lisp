(in-package #:bones.wam)

;;;; Opcodes
(defun* instruction-size ((opcode opcode))
  (:returns (integer 1 3))
  "Return the size of an instruction for the given opcode.

  The size includes one word for the opcode itself and one for each argument.

  "
  (eswitch (opcode)
    (+opcode-noop+ 1)

    (+opcode-get-structure-local+ 3)
    (+opcode-unify-variable-local+ 2)
    (+opcode-unify-variable-stack+ 2)
    (+opcode-unify-value-local+ 2)
    (+opcode-unify-value-stack+ 2)
    (+opcode-get-variable-local+ 3)
    (+opcode-get-variable-stack+ 3)
    (+opcode-get-value-local+ 3)
    (+opcode-get-value-stack+ 3)

    (+opcode-put-structure-local+ 3)
    (+opcode-set-variable-local+ 2)
    (+opcode-set-variable-stack+ 2)
    (+opcode-set-value-local+ 2)
    (+opcode-set-value-stack+ 2)
    (+opcode-put-variable-local+ 3)
    (+opcode-put-variable-stack+ 3)
    (+opcode-put-value-local+ 3)
    (+opcode-put-value-stack+ 3)

    (+opcode-call+ 2)
    (+opcode-proceed+ 1)
    (+opcode-allocate+ 2)
    (+opcode-deallocate+ 1)
    (+opcode-done+ 1)))


(defun* opcode-name ((opcode opcode))
  (:returns string)
  (eswitch (opcode)
    (+opcode-noop+ "NOOP")
    (+opcode-get-structure-local+ "GET-STRUCTURE")
    (+opcode-unify-variable-local+ "UNIFY-VARIABLE")
    (+opcode-unify-variable-stack+ "UNIFY-VARIABLE")
    (+opcode-unify-value-local+ "UNIFY-VALUE")
    (+opcode-unify-value-stack+ "UNIFY-VALUE")
    (+opcode-get-variable-local+ "GET-VARIABLE")
    (+opcode-get-variable-stack+ "GET-VARIABLE")
    (+opcode-get-value-local+ "GET-VALUE")
    (+opcode-get-value-stack+ "GET-VALUE")

    (+opcode-put-structure-local+ "PUT-STRUCTURE")
    (+opcode-set-variable-local+ "SET-VARIABLE")
    (+opcode-set-variable-stack+ "SET-VARIABLE")
    (+opcode-set-value-local+ "SET-VALUE")
    (+opcode-set-value-stack+ "SET-VALUE")
    (+opcode-put-variable-local+ "PUT-VARIABLE")
    (+opcode-put-variable-stack+ "PUT-VARIABLE")
    (+opcode-put-value-local+ "PUT-VALUE")
    (+opcode-put-value-stack+ "PUT-VALUE")

    (+opcode-call+ "CALL")
    (+opcode-proceed+ "PROCEED")
    (+opcode-allocate+ "ALLOCATE")
    (+opcode-deallocate+ "DEALLOCATE")
    (+opcode-done+ "DONE")))

(defun* opcode-short-name ((opcode opcode))
  (:returns string)
  (eswitch (opcode)
    (+opcode-noop+ "NOOP")

    (+opcode-get-structure-local+ "GETS")
    (+opcode-unify-variable-local+ "UVAR")
    (+opcode-unify-variable-stack+ "UVAR")
    (+opcode-unify-value-local+ "UVLU")
    (+opcode-unify-value-stack+ "UVLU")
    (+opcode-get-variable-local+ "GVAR")
    (+opcode-get-variable-stack+ "GVAR")
    (+opcode-get-value-local+ "GVLU")
    (+opcode-get-value-stack+ "GVLU")

    (+opcode-put-structure-local+ "PUTS")
    (+opcode-set-variable-local+ "SVAR")
    (+opcode-set-variable-stack+ "SVAR")
    (+opcode-set-value-local+ "SVLU")
    (+opcode-set-value-stack+ "SVLU")
    (+opcode-put-variable-local+ "PVAR")
    (+opcode-put-variable-stack+ "PVAR")
    (+opcode-put-value-local+ "PVLU")
    (+opcode-put-value-stack+ "PVLU")

    (+opcode-call+ "CALL")
    (+opcode-proceed+ "PROC")
    (+opcode-allocate+ "ALOC")
    (+opcode-deallocate+ "DEAL")
    (+opcode-done+ "DONE")))

