(in-package #:bones.wam)

(define-constant +cell-width+ 16
  :documentation "Number of bits in each cell.")

(define-constant +cell-tag-width+ 3
  :documentation "Number of bits reserved for cell type tags.")

(define-constant +cell-value-width+ (- +cell-width+ +cell-tag-width+)
  :documentation "Number of bits reserved for cell values.")

(define-constant +cell-tag-bitmask+ #b111
  :documentation "Bitmask for masking the cell type tags.")


(define-constant +code-word-size+ 16
  :documentation "Size (in bits) of each word in the code store.")

(define-constant +code-limit+ (expt 2 +code-word-size+)
  :documentation "Maximum size of the WAM code store.")

(define-constant +code-sentinel+ (1- +code-limit+)
  ; TODO: Should this sentinel value be 0 like everything else?
  :documentation "Sentinel value used in the PC and CP.")


(define-constant +tag-null+      #b000
  :documentation "An empty cell.")

(define-constant +tag-structure+ #b001
  :documentation "A structure cell.")

(define-constant +tag-reference+ #b010
  :documentation "A pointer to a cell.")

(define-constant +tag-functor+   #b011
  :documentation "A functor.")

(define-constant +tag-constant+  #b100
  :documentation "A constant (i.e. a 0-arity functor).")

(define-constant +tag-list+  #b101
  :documentation "A Prolog list.")


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


(define-constant +wildcard-symbol+ '?)


;;;; Opcodes
(defmacro define-opcodes (&rest symbols)
  `(progn
     ,@(loop :for c :from 0
             :for s :in symbols
             :collect `(define-constant ,s ,c))
     (define-constant +number-of-opcodes+ ,(length symbols))))

(define-opcodes
  +opcode-noop+

  ;; Program
  +opcode-get-structure+
  +opcode-get-variable-local+
  +opcode-get-variable-stack+
  +opcode-get-value-local+
  +opcode-get-value-stack+

  ;; Query
  +opcode-put-structure+
  +opcode-put-variable-local+
  +opcode-put-variable-stack+
  +opcode-put-value-local+
  +opcode-put-value-stack+

  ;; Subterm
  +opcode-unify-variable-local+
  +opcode-unify-variable-stack+
  +opcode-unify-value-local+
  +opcode-unify-value-stack+
  +opcode-unify-void+

  ;; Control
  +opcode-call+
  +opcode-dynamic-call+
  +opcode-proceed+
  +opcode-allocate+
  +opcode-deallocate+
  +opcode-done+
  +opcode-try+
  +opcode-retry+
  +opcode-trust+
  +opcode-cut+

  ;; Constants
  +opcode-get-constant+
  +opcode-put-constant+
  +opcode-unify-constant+

  ;; Lists
  +opcode-get-list+
  +opcode-put-list+)


;;;; Debug Config
(defparameter *off-by-one* nil)
