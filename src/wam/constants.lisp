(in-package #:bones.wam)

(defmacro define-constants (count-symbol &rest symbols)
  `(progn
     ,@(loop :for c :from 0
             :for s :in symbols
             :collect `(define-constant ,s ,c))
     (define-constant ,count-symbol ,(length symbols))))


(define-constant +code-word-size+ 60
  :documentation "Size (in bits) of each word in the code store.")

(define-constant +code-limit+ (expt 2 +code-word-size+)
  :documentation "Maximum size of the WAM code store.")

(define-constant +code-sentinel+ (1- +code-limit+)
  ; TODO: Should this sentinel value be 0 like everything else?
  :documentation "Sentinel value used in the PC and CP.")


(define-constants +number-of-cell-types+
  +cell-type-null+
  +cell-type-structure+
  +cell-type-reference+
  +cell-type-functor+
  +cell-type-constant+
  +cell-type-list+
  +cell-type-lisp-object+
  +cell-type-stack+)


(define-constant +register-count+ 2048
  :documentation "The number of local registers the WAM has available.")

(define-constant +maximum-arity+ 1024
  :documentation "The maximum allowed arity of functors.")


;; TODO Make all this shit configurable at runtime
(define-constant +stack-limit+ 4096
  :documentation "Maximum size of the WAM stack.")

(define-constant +stack-frame-size-limit+ (+ 7 +register-count+)
  :documentation "The maximum size, in stack frame words, that a stack frame could be.")


(define-constant +maximum-query-size+ 1024
  :documentation
  "The maximum size (in bytes of bytecode) a query may compile to.")

(define-constant +maximum-instruction-size+ 3
  :documentation
  "The maximum number of code words an instruction (including opcode) might be.")

(define-constant +code-query-start+ 0
  :documentation "The address in the code store where the query code begins.")

(define-constant +code-main-start+ +maximum-query-size+
  :documentation "The address in the code store where the main program code begins.")


(define-constant +stack-start+ +register-count+
  :documentation "The address in the store of the first cell of the stack.")

(define-constant +stack-end+ (+ +stack-start+ +stack-limit+)
  :documentation
  "The address in the store one past the last cell in the stack.")

(define-constant +heap-start+ +stack-end+
  :documentation "The address in the store of the first cell of the heap.")


(define-constant +trail-limit+ array-total-size-limit
  ;; TODO: should probably limit this to something more reasonable
  :documentation "The maximum number of variables that may exist in the trail.")

(define-constant +store-limit+ array-total-size-limit
  :documentation "Maximum size of the WAM store.")

(define-constant +heap-limit+ (- +store-limit+ +register-count+ +stack-limit+)
  ;; The heap gets whatever's left over after the registers and stack have taken
  ;; their chunk of memory.
  :documentation "Maximum size of the WAM heap.")

(define-constant +functor-limit+ array-total-size-limit
  ;; Functors are stored in a functor table.
  :documentation "The maximum number of functors the WAM can keep track of.")


(define-constant +wildcard-symbol+ '?)


;;;; Opcodes
(define-constants +number-of-opcodes+
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
  +opcode-subterm-variable-local+
  +opcode-subterm-variable-stack+
  +opcode-subterm-value-local+
  +opcode-subterm-value-stack+
  +opcode-subterm-void+

  ;; Control
  +opcode-jump+
  +opcode-call+
  +opcode-dynamic-jump+
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
  +opcode-subterm-constant+

  ;; Lists
  +opcode-get-list+
  +opcode-put-list+

  ;; Lisp Objects
  +opcode-get-lisp-object+
  +opcode-put-lisp-object+)


;;;; Debug Config
(defparameter *off-by-one* nil)
