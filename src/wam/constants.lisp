(in-package #:bones.wam)

(define-constant +cell-width+ 16
  :documentation "Number of bits in each cell.")

(define-constant +cell-tag-width+ 2
  :documentation "Number of bits reserved for cell type tags.")

(define-constant +cell-value-width+ (- +cell-width+ +cell-tag-width+)
  :documentation "Number of bits reserved for cell values.")

(define-constant +cell-tag-bitmask+ #b11
  :documentation "Bitmask for masking the cell type tags.")


(define-constant +code-word-size+ 16
  :documentation "Size (in bits) of each word in the code store.")

(define-constant +code-limit+ (expt 2 +code-word-size+)
  :documentation "Maximum size of the WAM code store.")

(define-constant +code-sentinal+ (1- +code-limit+)
  ; TODO: Should this sentinal value be 0 like everything else?
  :documentation "Sentinal value used in the PC and CP.")


(define-constant +tag-null+      #b00
  :documentation "An empty cell.")

(define-constant +tag-structure+ #b01
  :documentation "A structure cell.")

(define-constant +tag-reference+ #b10
  :documentation "A pointer to a cell.")

(define-constant +tag-functor+   #b11
  :documentation "A functor.")


(define-constant +register-count+ 2048
  :documentation "The number of local registers the WAM has available.")


(define-constant +maximum-arity+ 1024
  :documentation "The maximum allowed arity of functors.")

(define-constant +maximum-query-size+ 1024
  :documentation
  "The maximum size (in bytes of bytecode) a query may compile to.")


(define-constant +stack-limit+ 2048
  :documentation "Maximum size of the WAM stack.")

(define-constant +stack-frame-size-limit+ (+ 7 +register-count+)
  :documentation "The maximum size, in stack frame words, that a stack frame could be.")


(define-constant +stack-start+ +register-count+
  :documentation "The address in the store of the first cell of the stack.")

(define-constant +stack-end+ (+ +stack-start+ +stack-limit+)
  :documentation
  "The address in the store one past the last cell in the stack.")

(define-constant +heap-start+ +stack-end+
  :documentation "The address in the store of the first cell of the heap.")

(define-constant +trail-limit+ (expt 2 +cell-width+)
  ;; The trail's fill pointer is stored inside choice frames on the stack, so it
  ;; needs to be able to fit inside a stack word.  We don't tag it, though, so
  ;; we can technically use all of the cell bits if we want.
  ;;
  ;; TODO: should probably limit this to something more reasonable
  :documentation "The maximum number of variables that may exist in the trail.")


(define-constant +store-limit+ (expt 2 +cell-value-width+)
  ;; Reference cells need to be able to store a heap address in their value
  ;; bits, so that limits the amount of addressable space we've got to work
  ;; with.
  :documentation "Maximum size of the WAM store.")

(define-constant +heap-limit+ (- +store-limit+ +register-count+ +stack-limit+)
  ;; The heap gets whatever's left over after the registers and stack have taken
  ;; their chunk of memory.
  :documentation "Maximum size of the WAM heap.")


(define-constant +functor-limit+ (expt 2 +cell-value-width+)
  ;; Functors are referred to by their index into the functor array.  This index
  ;; is stored in the value part of functor cells.
  :documentation "The maximum number of functors the WAM can keep track of.")


;;;; Opcodes
;;; Program
(define-constant +opcode-noop+ 0)
(define-constant +opcode-get-structure-local+ 1)
(define-constant +opcode-unify-variable-local+ 2)
(define-constant +opcode-unify-variable-stack+ 3)
(define-constant +opcode-unify-value-local+ 4)
(define-constant +opcode-unify-value-stack+ 5)
(define-constant +opcode-get-variable-local+ 6)
(define-constant +opcode-get-variable-stack+ 7)
(define-constant +opcode-get-value-local+ 8)
(define-constant +opcode-get-value-stack+ 9)


;;; Query
(define-constant +opcode-put-structure-local+ 10)
(define-constant +opcode-set-variable-local+ 11)
(define-constant +opcode-set-variable-stack+ 12)
(define-constant +opcode-set-value-local+ 13)
(define-constant +opcode-set-value-stack+ 14)
(define-constant +opcode-put-variable-local+ 15)
(define-constant +opcode-put-variable-stack+ 16)
(define-constant +opcode-put-value-local+ 17)
(define-constant +opcode-put-value-stack+ 18)


;;; Control
(define-constant +opcode-call+ 19)
(define-constant +opcode-proceed+ 20)
(define-constant +opcode-allocate+ 21)
(define-constant +opcode-deallocate+ 22)
(define-constant +opcode-done+ 23)
(define-constant +opcode-try+ 24)
(define-constant +opcode-retry+ 25)
(define-constant +opcode-trust+ 26)


;;;; Debug Config
(defparameter *off-by-one* nil)
