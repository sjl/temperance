(in-package :temperance)

;;;; WAM
(defun allocate-wam-code (size)
  ;; The WAM bytecode is all stored in this array.  The first
  ;; `+maximum-query-size+` words are reserved for query bytecode, which will
  ;; get loaded in (overwriting the previous query) when making a query.
  ;; Everything after that is for the actual database.
  (make-array (+ +maximum-query-size+ size)
    :initial-element 0
    :element-type 'code-word))

(defun allocate-query-holder ()
  (make-array +maximum-query-size+
    :adjustable nil
    :initial-element 0
    :element-type 'code-word))


(defun allocate-wam-type-store (size)
  ;; The main WAM store(s) contain three separate blocks of values:
  ;;
  ;;     [0, +register-count+)        -> the local X_n registers
  ;;     [+stack-start+, +stack-end+) -> the stack
  ;;     [+heap-start+, ...)          -> the heap
  ;;
  ;; `+register-count+` and `+stack-start+` are the same number, and
  ;; `+stack-end+` and `+heap-start+` are the same number as well.
  (make-array (+ +register-count+
                 +stack-limit+
                 size) ; type array
    :initial-element +cell-type-null+
    :element-type 'cell-type))

(defun allocate-wam-value-store (size)
  (make-array (+ +register-count+
                 +stack-limit+
                 size)
    :initial-element 0
    :element-type 'cell-value))

(defun allocate-wam-unification-stack (size)
  (make-array size
    :fill-pointer 0
    :adjustable t
    :element-type 'store-index))

(defun allocate-wam-trail (size)
  (make-array size
    :fill-pointer 0
    :adjustable t
    :initial-element 0
    :element-type 'store-index))


(defstruct (wam (:constructor make-wam%))
  ;; Data
  (type-store
    (error "Type store required.")
    :type type-store
    :read-only t)
  (value-store
    (error "Value store required.")
    :type value-store
    :read-only t)
  (unification-stack
    (error "Unification stack required.")
    :type (vector store-index)
    :read-only t)
  (trail
    (error "Trail required.")
    :type (vector store-index)
    :read-only t)

  ;; Code
  (code
    (error "Code store required.")
    :type (simple-array code-word (*))
    :read-only t)
  (code-labels
    (make-array +maximum-arity+ :initial-element nil)
    :type (simple-array (or null hash-table))
    :read-only t)

  ;; Logic Stack
  (logic-stack nil :type list)
  (logic-pool nil :type list)

  ;; Unique registers
  (number-of-arguments     0                  :type arity)               ; NARGS
  (subterm                 +heap-start+       :type heap-index)          ; S
  (program-counter         0                  :type code-index)          ; P
  (code-pointer            +code-main-start+  :type code-index)          ; CODE
  (heap-pointer            (1+ +heap-start+)  :type heap-index)          ; H
  (stack-pointer           +stack-start+      :type stack-index)         ; SP
  (continuation-pointer    0                  :type code-index)          ; CP
  (environment-pointer     +stack-start+      :type environment-pointer) ; E
  (backtrack-pointer       +stack-start+      :type backtrack-pointer)   ; B
  (cut-pointer             +stack-start+      :type backtrack-pointer)   ; B0
  (heap-backtrack-pointer  +heap-start+       :type heap-index)          ; HB

  ;; Flags
  (fail        nil :type boolean)
  (backtracked nil :type boolean)
  (mode        nil :type (or null (member :read :write))))


(defmethod print-object ((wam wam) stream)
  (print-unreadable-object
      (wam stream :type t :identity t)
    (format stream "an wam")))


(defun make-wam (&key
                 (store-size (megabytes 10))
                 (code-size (megabytes 1)))
  (make-wam% :code (allocate-wam-code code-size)
             :type-store (allocate-wam-type-store store-size)
             :value-store (allocate-wam-value-store store-size)
             :unification-stack (allocate-wam-unification-stack 16)
             :trail (allocate-wam-trail 64)))


;;;; Store
;;; The main store of the WAM is split into two separate arrays:
;;;
;;; * An array of cell types, each a fixnum.
;;; * An array of cell values, each being a fixnum or a normal Lisp pointer.
;;;
;;; The contents of the value depend on the type of cell.
;;;
;;; NULL cells always have a value of zero.
;;;
;;; STRUCTURE cell values are an index into the store, describing where the
;;; structure starts.
;;;
;;; REFERENCE cell values are an index into the store, pointing at whatever the
;;; value is bound to.  Unbound variables contain their own store index as
;;; a value.
;;;
;;; FUNCTOR cell values are a pointer to a Lisp symbol.  The next cell after
;;; a FUNCTOR cell is always a LISP-OBJECT cell with the fuctor's arity.
;;;
;;; CONSTANT cells are the same as functor cells, except that they always happen
;;; to refer to functors with an arity of zero and therefore don't need to be
;;; followed by another cell for the arity.
;;;
;;; LIST cell values are an index into the store, pointing at the first of two
;;; consecutive cells.  The first cell is the car of the list, the second one is
;;; the cdr.
;;;
;;; LISP-OBJECT cell values are simply arbitrary objects in memory.  They are
;;; compared with `eql` during the unification process, so we don't actually
;;; care WHAT they are, exactly.
;;;
;;; STACK cell values are special cases.  The WAM's main store is a combination
;;; of the heap, the stack, and registers.  Heap cells (and registers) are those
;;; detailed above, but stack cells can also hold numbers like the continuation
;;; pointer.  We lump all the extra things together into one kind of cell.

(declaim (inline wam-store-type
                 wam-store-value
                 wam-set-store-cell!
                 wam-copy-store-cell!))


(defun wam-store-type (wam address)
  "Return the type of the cell at the given address."
  (aref (wam-type-store wam) address))

(defun wam-store-value (wam address)
  "Return the value of the cell at the given address."
  (aref (wam-value-store wam) address))


(defun wam-set-store-cell! (wam address type value)
  (setf (aref (wam-type-store wam) address) type
        (aref (wam-value-store wam) address) value))

(defun wam-copy-store-cell! (wam destination source)
  (wam-set-store-cell! wam
                       destination
                       (wam-store-type wam source)
                       (wam-store-value wam source)))


(defun wam-sanity-check-store-read (wam address)
  (declare (ignore wam))
  (when (= address +heap-start+)
    (error "Cannot read from heap address zero.")))


(macrolet ((define-unsafe (name return-type)
             `(progn
               (declaim (inline ,name))
               (defun ,name (wam address)
                 (the ,return-type (aref (wam-value-store wam) address))))))
  (define-unsafe %unsafe-null-value        (eql 0))
  (define-unsafe %unsafe-structure-value   store-index)
  (define-unsafe %unsafe-reference-value   store-index)
  (define-unsafe %unsafe-functor-value     fname)
  (define-unsafe %unsafe-constant-value    fname)
  (define-unsafe %unsafe-list-value        store-index)
  (define-unsafe %unsafe-lisp-object-value t)
  (define-unsafe %unsafe-stack-value       stack-word))


(defun %type-designator-constant (designator)
  (ecase designator
    (:null +cell-type-null+)
    (:structure +cell-type-structure+)
    (:reference +cell-type-reference+)
    (:functor +cell-type-functor+)
    (:constant +cell-type-constant+)
    (:list +cell-type-list+)
    (:lisp-object +cell-type-lisp-object+)
    ((t) t)))

(defun %type-designator-accessor (designator)
  (ecase designator
    (:null '%unsafe-null-value)
    (:structure '%unsafe-structure-value)
    (:reference '%unsafe-reference-value)
    (:functor '%unsafe-functor-value)
    (:constant '%unsafe-constant-value)
    (:list '%unsafe-list-value)
    (:lisp-object '%unsafe-lisp-object-value)))

(defun parse-cell-typecase-clause (wam address clause)
  "Parse a `cell-typecase` clause into the appropriate `ecase` clause."
  (destructuring-bind (binding . body) clause
    (destructuring-bind
        (type-designator &optional value-symbol secondary-value-symbol)
        (if (symbolp binding) (list binding) binding) ; normalize binding
      (let ((primary-let-binding
              (when value-symbol
                `((,value-symbol (,(%type-designator-accessor type-designator)
                                  ,wam ,address)))))
            (secondary-let-binding
              (when secondary-value-symbol
                `((,secondary-value-symbol
                   ,(ecase type-designator
                      (:functor
                       `(the arity (%unsafe-lisp-object-value ; yolo
                                     ,wam
                                     (1+ ,address))))))))))
        ; build the ecase clause (const ...body...)
        (list
          (%type-designator-constant type-designator)
          `(let (,@primary-let-binding
                 ,@secondary-let-binding)
            ,@body))))))

(defmacro cell-typecase ((wam address &optional address-symbol) &rest clauses)
  "Dispatch on the type of the cell at `address` in the WAM store.

  If `address-symbol` is given it will be bound to the result of evaluating
  `address` in the remainder of the form.

  The type of the cell will be matched against `clauses` much like `typecase`.

  Each clause should be of the form `(binding forms)`.

  Each binding can be either a simple cell type designator like `:reference`, or
  a list of this designator and a symbol to bind the cell's value to.  The
  symbol is bound with `let` around the `forms` and type-hinted appropriately
  (at least on SBCL).

  Example:

    (cell-typecase (wam (deref wam address) final-address)
      (:reference (bind final-address foo)
                  'it-is-a-reference)
      ((:constant c) (list 'it-is-the-constant c))
      (t 'unknown))

  "
  (once-only (wam address)
    `(progn
      (policy-cond:policy-if (or (= safety 3) (= debug 3))
        (wam-sanity-check-store-read ,wam ,address)
        nil)
      (let (,@(when address-symbol
                (list `(,address-symbol ,address))))
        (case (wam-store-type ,wam ,address)
          ,@(mapcar (curry #'parse-cell-typecase-clause wam address)
             clauses))))))


(defmacro cell-type= (type type-designator)
  `(= ,type ,(%type-designator-constant type-designator)))

(defmacro cell-type-p ((wam address) type-designator)
  `(cell-type=
    (wam-store-type ,wam ,address)
    ,type-designator))


;;;; Heap
;;; The WAM heap is all the memory left in the store after the local registers
;;; and stack have been accounted for.
;;;
;;; We reserve the first address in the heap as a sentinel, as an "unset" value
;;; for various pointers into the heap.

(declaim (inline wam-heap-pointer-unset-p wam-heap-push!))


(defun wam-heap-pointer-unset-p (wam address)
  (declare (ignore wam))
  (= address +heap-start+))

(defun wam-heap-push! (wam type value)
  "Push the cell onto the WAM heap and increment the heap pointer.

  Returns the address it was pushed to.

  "
  (let ((heap-pointer (wam-heap-pointer wam)))
    (if (>= heap-pointer +store-limit+) ; todo: respect actual size...
      (error "WAM heap exhausted.")
      (progn
        (wam-set-store-cell! wam heap-pointer type value)
        (incf (wam-heap-pointer wam))
        heap-pointer))))


;;;; Trail
(declaim (inline wam-trail-pointer
                 (setf wam-trail-pointer)
                 wam-trail-value
                 (setf wam-trail-value)))


(defun wam-trail-pointer (wam)
  "Return the current trail pointer of the WAM."
  (fill-pointer (wam-trail wam)))

(defun (setf wam-trail-pointer) (new-value wam)
  (setf (fill-pointer (wam-trail wam)) new-value))


(defun wam-trail-push! (wam address)
  "Push `address` onto the trail.

  Returns the address and the trail address it was pushed to.

  "
  (let ((trail (wam-trail wam)))
    (if (= +trail-limit+ (fill-pointer trail))
      (error "WAM trail exhausted.")
      (values address (vector-push-extend address trail)))))

(defun wam-trail-pop! (wam)
  "Pop the top address off the trail and return it."
  (vector-pop (wam-trail wam)))

(defun wam-trail-value (wam address)
  ;; TODO: can we really not just pop, or is something else gonna do something
  ;; fucky with the trail?
  "Return the element (a heap index) in the WAM trail at `address`."
  (aref (wam-trail wam) address))

(defun (setf wam-trail-value) (new-value wam address)
  (setf (aref (wam-trail wam) address) new-value))


;;;; Stack
;;; The stack is stored as a fixed-length hunk of the main WAM store array,
;;; between the local register and the heap, with small glitch: we reserve the
;;; first word of the stack (address `+stack-start`) to mean "uninitialized", so
;;; we have a nice sentinel value for the various pointers into the stack.

(declaim (inline assert-inside-stack
                 wam-stack-ensure-size
                 wam-stack-word
                 (setf wam-stack-word)
                 wam-backtrack-pointer-unset-p
                 wam-environment-pointer-unset-p))


(defun assert-inside-stack (wam address)
  (declare (ignorable wam address))
  (policy-cond:policy-cond
    ((>= debug 2)
     (progn
       (assert (<= +stack-start+ address (1- +stack-end+)) ()
         "Cannot access stack cell at address ~X (outside the stack range ~X to ~X)"
         address +stack-start+ +stack-end+)
       (assert (not (= +stack-start+ address)) ()
         "Cannot access stack address zero.")))
    ((>= safety 1)
     (when (not (< +stack-start+ address +stack-end+))
       (error "Stack bounds crossed.  Game over.")))
    (t nil)) ; wew lads
  nil)

(defun wam-stack-ensure-size (wam address)
  "Ensure the WAM stack is large enough to be able to write to `address`."
  (assert-inside-stack wam address))


(defun wam-stack-word (wam address)
  "Return the stack word at the given address."
  (assert-inside-stack wam address)
  (%unsafe-stack-value wam address))

(defun (setf wam-stack-word) (new-value wam address)
  (assert-inside-stack wam address)
  (wam-set-store-cell! wam address +cell-type-stack+ new-value))


(defun wam-backtrack-pointer-unset-p
    (wam &optional (backtrack-pointer (wam-backtrack-pointer wam)))
  (= backtrack-pointer +stack-start+))

(defun wam-environment-pointer-unset-p
    (wam &optional (environment-pointer (wam-environment-pointer wam)))
  (= environment-pointer +stack-start+))


;;; Stack frames are laid out like so:
;;;
;;;     |PREV|
;;;     | CE | <-- environment-pointer
;;;     | CP |
;;;     | B0 |
;;;     | N  |
;;;     | Y0 |
;;;     | .. |
;;;     | Yn |
;;;     |NEXT| <-- fill-pointer

(declaim (inline wam-stack-frame-ce
                 wam-stack-frame-cp
                 wam-stack-frame-cut
                 wam-stack-frame-n
                 wam-stack-frame-size
                 wam-stack-frame-argument-address
                 wam-set-stack-frame-argument!))


(defun wam-stack-frame-ce (wam &optional (e (wam-environment-pointer wam)))
  (wam-stack-word wam e))

(defun wam-stack-frame-cp (wam &optional (e (wam-environment-pointer wam)))
  (wam-stack-word wam (1+ e)))

(defun wam-stack-frame-cut (wam &optional (e (wam-environment-pointer wam)))
  (wam-stack-word wam (+ 2 e)))

(defun wam-stack-frame-n (wam &optional (e (wam-environment-pointer wam)))
  (wam-stack-word wam (+ 3 e)))


(defun wam-stack-frame-argument-address
    (wam n &optional (e (wam-environment-pointer wam)))
  (+ 4 n e))

(defun wam-set-stack-frame-argument!  (wam n type value
                                       &optional (e (wam-environment-pointer wam)))
  (wam-set-store-cell! wam (wam-stack-frame-argument-address wam n e)
                       type value))

(defun wam-copy-to-stack-frame-argument!  (wam n source
                                            &optional (e (wam-environment-pointer wam)))
  (wam-copy-store-cell! wam (wam-stack-frame-argument-address wam n e)
                        source))


(defun wam-stack-frame-size (wam &optional (e (wam-environment-pointer wam)))
  "Return the size of the stack frame starting at environment pointer `e`."
  (+ (wam-stack-frame-n wam e) 4))


;;; Choice point frames are laid out like so:
;;;
;;;         |PREV|
;;;       0 | N  | number of arguments          <-- backtrack-pointer
;;;       1 | CE | continuation environment
;;;       2 | CP | continuation pointer
;;;       3 | CB | previous choice point
;;;       4 | BP | next clause
;;;       5 | TR | trail pointer
;;;       6 | H  | heap pointer
;;;       7 | CC | saved cut pointer
;;;       8 | A0 |
;;;         | .. |
;;;     8+n | An |
;;;         |NEXT| <-- environment-pointer
;;;
;;; This is a bit different than the book.  We stick the args at the end of the
;;; frame instead of the beginning so it's easier to retrieve the other values.

(declaim (inline wam-stack-choice-n
                 wam-stack-choice-ce
                 wam-stack-choice-cp
                 wam-stack-choice-cb
                 wam-stack-choice-cc
                 wam-stack-choice-bp
                 wam-stack-choice-tr
                 wam-stack-choice-h
                 wam-stack-choice-size
                 wam-stack-choice-argument-address
                 wam-set-stack-choice-argument!
                 wam-copy-to-stack-choice-argument!))


(defun wam-stack-choice-n (wam &optional (b (wam-backtrack-pointer wam)))
  (wam-stack-word wam b))

(defun wam-stack-choice-ce (wam &optional (b (wam-backtrack-pointer wam)))
  (wam-stack-word wam (+ b 1)))

(defun wam-stack-choice-cp (wam &optional (b (wam-backtrack-pointer wam)))
  (wam-stack-word wam (+ b 2)))

(defun wam-stack-choice-cb (wam &optional (b (wam-backtrack-pointer wam)))
  (wam-stack-word wam (+ b 3)))

(defun wam-stack-choice-bp (wam &optional (b (wam-backtrack-pointer wam)))
  (wam-stack-word wam (+ b 4)))

(defun wam-stack-choice-tr (wam &optional (b (wam-backtrack-pointer wam)))
  (wam-stack-word wam (+ b 5)))

(defun wam-stack-choice-h (wam &optional (b (wam-backtrack-pointer wam)))
  (wam-stack-word wam (+ b 6)))

(defun wam-stack-choice-cc (wam &optional (b (wam-backtrack-pointer wam)))
  (wam-stack-word wam (+ b 7)))


(defun wam-stack-choice-argument-address
    (wam n &optional (b (wam-backtrack-pointer wam)))
  (+ 8 n b))

(defun wam-set-stack-choice-argument! (wam n type value
                                        &optional (b (wam-backtrack-pointer wam)))
  (wam-set-store-cell! wam (wam-stack-choice-argument-address wam n b)
                       type value))

(defun wam-copy-to-stack-choice-argument!  (wam n source
                                             &optional (b (wam-backtrack-pointer wam)))
  (wam-copy-store-cell! wam (wam-stack-choice-argument-address wam n b)
                        source))


(defun wam-stack-choice-size (wam &optional (b (wam-backtrack-pointer wam)))
  "Return the size of the choice frame starting at backtrack pointer `b`."
  (+ (wam-stack-choice-n wam b) 8))


(defun wam-stack-top (wam)
  "Return the top of the stack.

  This is the first place it's safe to overwrite in the stack.

  "
  ;; The book is wrong here -- it looks up the "current frame size" to
  ;; determine where the next frame should start, but on the first allocation
  ;; there IS no current frame so it looks at garbage.  Fuckin' great.
  (let ((e (wam-environment-pointer wam))
        (b (wam-backtrack-pointer wam)))
    (cond
      ((and (wam-backtrack-pointer-unset-p wam b)
            (wam-environment-pointer-unset-p wam e)) ; first allocation
       (1+ +stack-start+))
      ((> e b) ; the last thing on the stack is a frame
       (+ e (wam-stack-frame-size wam e)))
      (t ; the last thing on the stack is a choice point
       (+ b (wam-stack-choice-size wam b))))))


;;;; Resetting
(defun wam-truncate-heap! (wam)
  ;; todo: null out the heap once we're storing live objects
  (setf (wam-heap-pointer wam) (1+ +heap-start+)))

(defun wam-truncate-trail! (wam)
  (setf (fill-pointer (wam-trail wam)) 0))

(defun wam-truncate-unification-stack! (wam)
  (setf (fill-pointer (wam-unification-stack wam)) 0))

(defun wam-reset-local-registers! (wam)
  (fill (wam-type-store wam) +cell-type-null+ :start 0 :end +register-count+)
  (fill (wam-value-store wam) 0 :start 0 :end +register-count+))

(defun wam-reset! (wam)
  (wam-truncate-heap! wam)
  (wam-truncate-trail! wam)
  (wam-truncate-unification-stack! wam)
  (policy-cond:policy-if (>= debug 2)
    ;; todo we can't elide this once we start storing live objects... :(
    (wam-reset-local-registers! wam)
    nil) ; fuck it
  (fill (wam-code wam) 0 :start 0 :end +maximum-query-size+)
  (setf (wam-program-counter wam) 0
        (wam-continuation-pointer wam) 0
        (wam-environment-pointer wam) +stack-start+
        (wam-backtrack-pointer wam) +stack-start+
        (wam-cut-pointer wam) +stack-start+
        (wam-heap-backtrack-pointer wam) +heap-start+
        (wam-backtracked wam) nil
        (wam-fail wam) nil
        (wam-subterm wam) +heap-start+
        (wam-mode wam) nil))


;;;; Code
;;; The WAM needs to be able to look up predicates at runtime.  To do this we
;;; keep a data structure that maps a functor and arity to a location in the
;;; code store.
;;;
;;; This data structure is an array, with the arity we're looking up being the
;;; position.  At that position will be a hash tables of the functor symbols to
;;; the locations.
;;;
;;; Each arity's table will be created on-the-fly when it's first needed.

(defun retrieve-instruction (code-store address)
  "Return the full instruction at the given address in the code store."
  (make-array (instruction-size (aref code-store address))
    :displaced-to code-store
    :displaced-index-offset address
    :adjustable nil
    :element-type 'code-word))


(defun wam-code-label (wam functor arity)
  (let ((atable (aref (wam-code-labels wam) arity)))
    (when atable
      (values (gethash functor atable)))))

(defun (setf wam-code-label) (new-value wam functor arity)
  (setf (gethash functor (ensure-aref (wam-code-labels wam) arity
                                      (make-hash-table :test 'eq)))
        new-value))

(defun wam-code-label-remove! (wam functor arity)
  (let ((atable (aref (wam-code-labels wam) arity)))
    (when atable
      ;; todo: remove the table entirely when empty?
      (remhash functor atable))))


(declaim (ftype (function (wam query-code-holder query-size)
                          (values null &optional))
                wam-load-query-code!))
(defun wam-load-query-code! (wam query-code query-size)
  (setf (subseq (wam-code wam) 0 query-size) query-code)
  nil)


;;;; Logic Stack
;;; The logic stack is stored as a simple list in the WAM.  `logic-frame`
;;; structs are pushed and popped from this list as requested.
;;;
;;; There's one small problem: logic frames need to keep track of which
;;; predicates are awaiting compilation, and the best data structure for that is
;;; a hash table.  But hash tables are quite expensive to allocate when you're
;;; pushing and popping tons of frames per second.  So the WAM also keeps a pool
;;; of logic frames to reuse, which lets us simply `clrhash` in between instead
;;; of having to allocate a brand new hash table.

(declaim (inline assert-logic-frame-poppable))


(defstruct logic-frame
  (start 0 :type code-index)
  (final nil :type boolean)
  (predicates (make-hash-table :test 'equal) :type hash-table))


(defun wam-logic-pool-release (wam frame)
  (with-slots (start final predicates) frame
    (clrhash predicates)
    (setf start 0 final nil))
  (push frame (wam-logic-pool wam))
  nil)

(defun wam-logic-pool-request (wam)
  (or (pop (wam-logic-pool wam))
      (make-logic-frame)))


(defun wam-current-logic-frame (wam)
  (first (wam-logic-stack wam)))

(defun wam-logic-stack-empty-p (wam)
  (not (wam-current-logic-frame wam)))


(defun wam-logic-open-p (wam)
  (let ((frame (wam-current-logic-frame wam)))
    (and frame (not (logic-frame-final frame)))))

(defun wam-logic-closed-p (wam)
  (not (wam-logic-open-p wam)))


(defun wam-push-logic-frame! (wam)
  (assert (wam-logic-closed-p wam) ()
    "Cannot push logic frame unless the logic stack is closed.")
  (let ((frame (wam-logic-pool-request wam)))
    (setf (logic-frame-start frame)
          (wam-code-pointer wam))
    (push frame (wam-logic-stack wam)))
  nil)

(defun assert-logic-frame-poppable (wam)
  (let ((logic-stack (wam-logic-stack wam)))
    (policy-cond:policy-if (or (> safety 1) (> debug 0) (< speed 3))
      ;; Slow
      (progn
        (assert logic-stack ()
          "Cannot pop logic frame from an empty logic stack.")
        (assert (logic-frame-final (first logic-stack)) ()
          "Cannot pop unfinalized logic frame."))
      ;; Fast
      (when (or (not logic-stack)
                (not (logic-frame-final (first logic-stack))))
        (error "Cannot pop logic frame.")))))

(defun wam-pop-logic-frame! (wam)
  (with-slots (logic-stack) wam
    (assert-logic-frame-poppable wam)
    (let ((frame (pop logic-stack)))
      (setf (wam-code-pointer wam)
            (logic-frame-start frame))
      (loop :for (functor . arity)
            :being :the hash-keys :of (logic-frame-predicates frame)
            :do (wam-code-label-remove! wam functor arity))
      (wam-logic-pool-release wam frame)))
  nil)


(defun assert-label-not-already-compiled (wam clause functor arity)
  (assert (not (wam-code-label wam functor arity))
      ()
    "Cannot add clause ~S because its predicate has preexisting compiled code."
    clause))

(defun wam-logic-frame-add-clause! (wam clause)
  (assert (wam-logic-open-p wam) ()
    "Cannot add clause ~S without an open logic stack frame."
    clause)

  (multiple-value-bind (functor arity) (find-predicate clause)
    (assert-label-not-already-compiled wam clause functor arity)
    (enqueue clause (ensure-gethash
                      (cons functor arity)
                      (logic-frame-predicates (wam-current-logic-frame wam))
                      (make-queue))))
  nil)


(defun wam-finalize-logic-frame! (wam)
  (assert (wam-logic-open-p wam) ()
    "There is no logic frame waiting to be finalized.")
  (with-slots (predicates final)
      (wam-current-logic-frame wam)
    (loop :for clauses :being :the hash-values :of predicates
          ;; circular dep on the compiler here, ugh.
          :do (compile-rules wam (queue-contents clauses)))
    (setf final t))
  nil)


;;;; Registers
;;; The WAM has two types of registers:
;;;
;;; * Local/temporary/arguments registers live at the beginning of the WAM
;;;   memory store.
;;;
;;; * Stack/permanent registers live on the stack, and need some extra math to
;;;   find their location.
;;;
;;; Registers are typically denoted by their "register index", which is just
;;; their number.  Hoever, the bytecode needs to be able to distinguish between
;;; local and stack registers.  To do this we just make separate opcodes for
;;; each kind.  This is ugly, but it lets us figure things out at compile time
;;; instead of runtime, and register references happen A LOT at runtime.
;;;
;;; As for the CONTENTS of registers: a register (regardless of type) always
;;; contains a cell.  The book is maddeningly unclear on this in a bunch of
;;; ways.  I will list them here so maybe you can feel a bit of my suffering
;;; through these bytes of text.
;;;
;;; The first thing the book says about registers is "registers have the same
;;; format as heap cells".  Okay, fine.  The *very next diagram* shows "register
;;; assignments" that appear to put things that are very much *not* heap cells
;;; into registers!
;;;
;;; After a bit of puttering you realize that the diagram is referring only to
;;; the compilation, not what's *actually* stored in these registers at runtime.
;;; You move on and see some pseudocode that contains `X_i <- HEAP[H]` which
;;; confirms that his original claim was accurate, and registers are actually
;;; (copies of) heap cells.  Cool.
;;;
;;; Then you move on and see the definition of `deref(a : address)` and note
;;; that it takes an *address* as an argument.  On the next page you see
;;; `deref(X_i)` and wait what the fuck, a register is an *address* now?  You
;;; scan down the page and see `HEAP[H] <- X_i` which means no wait it's a cell
;;; again.
;;;
;;; After considering depositing your laptop into the nearest toilet and
;;; becoming a sheep farmer, you conclude a few things:
;;;
;;; 1. The book's code won't typecheck.
;;; 2. The author is playing fast and loose with `X_i` -- sometimes it seems to
;;;    be used as an address, sometimes as a cell.
;;; 3. The author never bothers to nail down exactly what is inside the fucking
;;;    things, which is a problem because of #2.
;;;
;;; If you're like me (painfully unlucky), you took a wild guess and decided to
;;; implement registers as containing *addresses*, i.e., indexes into the
;;; heap, figuring that if you were wrong it would soon become apparent.
;;;
;;; WELL it turns out that you can get all the way to CHAPTER FIVE with
;;; registers implemented as addresses, at which point you hit a wall and need
;;; to spend a few hours refactoring a giant chunk of your code and writing
;;; angry comments in your source code.
;;;
;;; Hopefully I can save someone else this misery by leaving you with this:
;;;     ____  _____________________________________  _____    ___    ____  ______   ______________    __   _____
;;;    / __ \/ ____/ ____/  _/ ___/_  __/ ____/ __ \/ ___/   /   |  / __ \/ ____/  / ____/ ____/ /   / /  / ___/
;;;   / /_/ / __/ / / __ / / \__ \ / / / __/ / /_/ /\__ \   / /| | / /_/ / __/    / /   / __/ / /   / /   \__ \
;;;  / _, _/ /___/ /_/ // / ___/ // / / /___/ _, _/___/ /  / ___ |/ _, _/ /___   / /___/ /___/ /___/ /______/ /
;;; /_/ |_/_____/\____/___//____//_/ /_____/_/ |_|/____/  /_/  |_/_/ |_/_____/   \____/_____/_____/_____/____/

(declaim (inline wam-set-local-register!
                 wam-set-stack-register!
                 wam-local-register-address
                 wam-stack-register-address
                 wam-local-register-type
                 wam-stack-register-type
                 wam-local-register-value
                 wam-stack-register-value
                 wam-copy-to-local-register!
                 wam-copy-to-stack-register!
                 wam-local-register-address
                 wam-stack-register-address))


(defun wam-local-register-address (wam register)
  (declare (ignore wam))
  register)

(defun wam-stack-register-address (wam register)
  (wam-stack-frame-argument-address wam register))


(defun wam-local-register-type (wam register)
  (wam-store-type wam (wam-local-register-address wam register)))

(defun wam-stack-register-type (wam register)
  (wam-store-type wam (wam-stack-register-address wam register)))


(defun wam-local-register-value (wam register)
  (wam-store-value wam (wam-local-register-address wam register)))

(defun wam-stack-register-value (wam register)
  (wam-store-value wam (wam-stack-register-address wam register)))


(defun wam-set-local-register! (wam address type value)
  (wam-set-store-cell! wam (wam-local-register-address wam address)
                       type value))

(defun wam-set-stack-register! (wam address type value)
  (wam-set-stack-frame-argument! wam address type value))


(defun wam-copy-to-local-register! (wam destination source)
  (wam-copy-store-cell! wam (wam-local-register-address wam destination) source))

(defun wam-copy-to-stack-register! (wam destination source)
  (wam-copy-store-cell! wam (wam-stack-register-address wam destination) source))


;;;; Unification Stack
(declaim (inline wam-unification-stack-push!
                 wam-unification-stack-pop!
                 wam-unification-stack-empty-p))


(defun wam-unification-stack-push! (wam address1 address2)
  (vector-push-extend address1 (wam-unification-stack wam))
  (vector-push-extend address2 (wam-unification-stack wam)))

(defun wam-unification-stack-pop! (wam)
  (vector-pop (wam-unification-stack wam)))

(defun wam-unification-stack-empty-p (wam)
  (zerop (length (wam-unification-stack wam))))
