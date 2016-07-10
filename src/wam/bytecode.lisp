(in-package #:bones.wam)

;;;; Opcodes
(declaim (inline instruction-size))
(defun* instruction-size ((opcode opcode))
  (:returns (integer 1 3))
  "Return the size of an instruction for the given opcode.

  The size includes one word for the opcode itself and one for each argument.

  "
  (eswitch (opcode)
    ;; TODO: make this thing a jump table somehow...
    (+opcode-noop+ 1)

    (+opcode-get-structure+ 3)
    (+opcode-get-variable-local+ 3)
    (+opcode-get-variable-stack+ 3)
    (+opcode-get-value-local+ 3)
    (+opcode-get-value-stack+ 3)

    (+opcode-put-structure+ 3)
    (+opcode-put-variable-local+ 3)
    (+opcode-put-variable-stack+ 3)
    (+opcode-put-value-local+ 3)
    (+opcode-put-value-stack+ 3)

    (+opcode-subterm-variable-local+ 2)
    (+opcode-subterm-variable-stack+ 2)
    (+opcode-subterm-value-local+ 2)
    (+opcode-subterm-value-stack+ 2)
    (+opcode-subterm-void+ 2)

    (+opcode-call+ 2)
    (+opcode-dynamic-call+ 1)
    (+opcode-proceed+ 1)
    (+opcode-allocate+ 2)
    (+opcode-deallocate+ 1)
    (+opcode-done+ 1)
    (+opcode-try+ 2)
    (+opcode-retry+ 2)
    (+opcode-trust+ 1)
    (+opcode-cut+ 1)

    (+opcode-get-constant+ 3)
    (+opcode-put-constant+ 3)
    (+opcode-subterm-constant+ 2)

    (+opcode-get-list+ 2)
    (+opcode-put-list+ 2)))


(defun* opcode-name ((opcode opcode))
  (:returns string)
  (eswitch (opcode)
    (+opcode-noop+ "NOOP")

    (+opcode-get-structure+ "GET-STRUCTURE")
    (+opcode-get-variable-local+ "GET-VARIABLE")
    (+opcode-get-variable-stack+ "GET-VARIABLE")
    (+opcode-get-value-local+ "GET-VALUE")
    (+opcode-get-value-stack+ "GET-VALUE")

    (+opcode-put-structure+ "PUT-STRUCTURE")
    (+opcode-put-variable-local+ "PUT-VARIABLE")
    (+opcode-put-variable-stack+ "PUT-VARIABLE")
    (+opcode-put-value-local+ "PUT-VALUE")
    (+opcode-put-value-stack+ "PUT-VALUE")

    (+opcode-subterm-variable-local+ "SUBTERM-VARIABLE")
    (+opcode-subterm-variable-stack+ "SUBTERM-VARIABLE")
    (+opcode-subterm-value-local+ "SUBTERM-VALUE")
    (+opcode-subterm-value-stack+ "SUBTERM-VALUE")
    (+opcode-subterm-void+ "SUBTERM-VOID")

    (+opcode-call+ "CALL")
    (+opcode-dynamic-call+ "DYNAMIC-CALL")
    (+opcode-proceed+ "PROCEED")
    (+opcode-allocate+ "ALLOCATE")
    (+opcode-deallocate+ "DEALLOCATE")
    (+opcode-done+ "DONE")
    (+opcode-try+ "TRY")
    (+opcode-retry+ "RETRY")
    (+opcode-trust+ "TRUST")
    (+opcode-cut+ "CUT")

    (+opcode-get-constant+ "GET-CONSTANT")
    (+opcode-put-constant+ "PUT-CONSTANT")
    (+opcode-subterm-constant+ "SUBTERM-CONSTANT")

    (+opcode-get-list+ "GET-LIST")
    (+opcode-put-list+ "PUT-LIST")))

(defun* opcode-short-name ((opcode opcode))
  (:returns string)
  (eswitch (opcode)
    (+opcode-noop+ "NOOP")

    (+opcode-get-structure+ "GETS")
    (+opcode-get-variable-local+ "GVAR")
    (+opcode-get-variable-stack+ "GVAR")
    (+opcode-get-value-local+ "GVLU")
    (+opcode-get-value-stack+ "GVLU")

    (+opcode-put-structure+ "PUTS")
    (+opcode-put-variable-local+ "PVAR")
    (+opcode-put-variable-stack+ "PVAR")
    (+opcode-put-value-local+ "PVLU")
    (+opcode-put-value-stack+ "PVLU")

    (+opcode-subterm-variable-local+ "SVAR")
    (+opcode-subterm-variable-stack+ "SVAR")
    (+opcode-subterm-value-local+ "SVLU")
    (+opcode-subterm-value-stack+ "SVLU")
    (+opcode-subterm-void+ "SVOI")

    (+opcode-call+ "CALL")
    (+opcode-dynamic-call+ "DYCL")
    (+opcode-proceed+ "PROC")
    (+opcode-allocate+ "ALOC")
    (+opcode-deallocate+ "DEAL")
    (+opcode-done+ "DONE")
    (+opcode-try+ "TRYM")
    (+opcode-retry+ "RTRY")
    (+opcode-trust+ "TRST")
    (+opcode-cut+ "CUTT")

    (+opcode-get-constant+ "GCON")
    (+opcode-put-constant+ "PCON")
    (+opcode-subterm-constant+ "UCON")

    (+opcode-get-list+ "GLST")
    (+opcode-put-list+ "PLST")))

