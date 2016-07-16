(in-package #:bones.wam)


;;;; Opcodes
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

    (+opcode-jump+ "JUMP")
    (+opcode-call+ "CALL")
    (+opcode-dynamic-jump+ "DYNAMIC-JUMP")
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
    (+opcode-put-list+ "PUT-LIST")

    (+opcode-get-lisp-object+ "GET-LISP-OBJECT")
    (+opcode-put-lisp-object+ "PUT-LISP-OBJECT")))

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

    (+opcode-jump+ "JUMP")
    (+opcode-call+ "CALL")
    (+opcode-dynamic-jump+ "DYJP")
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
    (+opcode-put-list+ "PLST")

    (+opcode-get-lisp-object+ "GLOB")
    (+opcode-put-lisp-object+ "PLOB")))


;;;; Instructions
(define-lookup instruction-size (opcode opcode instruction-size 0)
  "Return the size of an instruction for the given opcode.

  The size includes one word for the opcode itself and one for each argument.

  "
  (#.+opcode-noop+ 1)

  (#.+opcode-get-structure+ 4)
  (#.+opcode-get-variable-local+ 3)
  (#.+opcode-get-variable-stack+ 3)
  (#.+opcode-get-value-local+ 3)
  (#.+opcode-get-value-stack+ 3)

  (#.+opcode-put-structure+ 4)
  (#.+opcode-put-variable-local+ 3)
  (#.+opcode-put-variable-stack+ 3)
  (#.+opcode-put-value-local+ 3)
  (#.+opcode-put-value-stack+ 3)

  (#.+opcode-subterm-variable-local+ 2)
  (#.+opcode-subterm-variable-stack+ 2)
  (#.+opcode-subterm-value-local+ 2)
  (#.+opcode-subterm-value-stack+ 2)
  (#.+opcode-subterm-void+ 2)

  (#.+opcode-jump+ 3)
  (#.+opcode-call+ 3)
  (#.+opcode-dynamic-jump+ 1)
  (#.+opcode-dynamic-call+ 1)
  (#.+opcode-proceed+ 1)
  (#.+opcode-allocate+ 2)
  (#.+opcode-deallocate+ 1)
  (#.+opcode-done+ 1)
  (#.+opcode-try+ 2)
  (#.+opcode-retry+ 2)
  (#.+opcode-trust+ 1)
  (#.+opcode-cut+ 1)

  (#.+opcode-get-constant+ 3)
  (#.+opcode-put-constant+ 3)
  (#.+opcode-subterm-constant+ 2)

  (#.+opcode-get-list+ 2)
  (#.+opcode-put-list+ 2)

  (#.+opcode-get-lisp-object+ 3)
  (#.+opcode-put-lisp-object+ 3))


;;;; Cells
(define-lookup cell-type-name (type cell-type string "")
  "Return the full name of a cell type."
  (#.+cell-type-null+ "NULL")
  (#.+cell-type-structure+ "STRUCTURE")
  (#.+cell-type-reference+ "REFERENCE")
  (#.+cell-type-functor+ "FUNCTOR")
  (#.+cell-type-constant+ "CONSTANT")
  (#.+cell-type-list+ "LIST")
  (#.+cell-type-lisp-object+ "LISP-OBJECT")
  (#.+cell-type-stack+ "STACK"))

(define-lookup cell-type-short-name (type cell-type string "")
  "Return the short name of a cell type."
  (#.+cell-type-null+ "NUL")
  (#.+cell-type-structure+ "STR")
  (#.+cell-type-reference+ "REF")
  (#.+cell-type-functor+ "FUN")
  (#.+cell-type-constant+ "CON")
  (#.+cell-type-list+ "LIS")
  (#.+cell-type-lisp-object+ "OBJ")
  (#.+cell-type-stack+ "STK"))

