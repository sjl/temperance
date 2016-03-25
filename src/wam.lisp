(in-package #:bones.wam)

(declaim (optimize (safety 3) (debug 3)))

;;;; Utilities
(defun pb (b)
  (format t "~B~%" b))


;;;; Constants
(define-constant +cell-width+ 16
  :documentation "Number of bits in each heap cell.")

(define-constant +cell-tag-width+ 2
  :documentation "Number of bits reserved for cell type tags.")

(define-constant +cell-value-width+ (- +cell-width+ +cell-tag-width+)
  :documentation "Number of bits reserved for cell values.")

(define-constant +cell-tag-bitmask+ #b11
  :documentation "Bitmask for masking the cell type tags.")


(define-constant +tag-null+      #b00
  :documentation "An empty cell.")

(define-constant +tag-structure+ #b01
  :documentation "A structure cell.")

(define-constant +tag-reference+ #b10
  :documentation "A pointer to a cell.")

(define-constant +tag-functor+   #b11
  :documentation "A functor.")


(define-constant +functor-arity-width+ 4
  :documentation "Number of bits dedicated to functor arity.")

(define-constant +functor-arity-bitmask+ #b1111
  :documentation "Bitmask for the functor arity bits.")


(define-constant +register-count+ 16
  :documentation "The number of registers the WAM has available.")

(define-constant +maximum-arity+ (1- (expt 2 +functor-arity-width+))
  :documentation "The maximum allowed arity of functors.")


;;;; Heap Cells
;;; The cells of the WAM are essentially N bit bytes, with different chunks of
;;; bits representing different things.  All cells have type tag bits in the
;;; low-order bits and their value in the higher-order bits:
;;;
;;;   value         type
;;;   vvvvvvvvvvvvvvTT
;;;
;;; The contents of the value depend on the type of cell.
;;;
;;; NULL cells always have a value of zero.
;;;
;;; STRUCTURE cell values are an index into the heap, describing where the
;;; structure starts.
;;;
;;; REFERENCE cell values are an index into the heap, pointing at whatever the
;;; value is bound to.  Unbound variables contain their own heap index as
;;; a value.
;;;
;;; FUNCTOR cell values are again split into two chunks of bits:
;;;
;;;   index     arity
;;;   iiiiiiiiiiAAAA
;;;
;;; The index is the index into the WAM's functor table where this functor's
;;; symbol lives.  Arity is the arity of the functor.
(deftype heap-cell ()
  `(unsigned-byte ,+cell-width+))

(deftype heap-cell-tag ()
  `(unsigned-byte ,+cell-tag-width+))

(deftype heap-cell-value ()
  `(unsigned-byte ,+cell-value-width+))


(deftype heap-index ()
  `(integer 0 ,(1- array-total-size-limit)))

(deftype register-index ()
  `(integer 0 ,(1- +register-count+)))

(deftype functor-index ()
  `(integer 0 ,(1- array-total-size-limit)))

(deftype arity ()
  `(integer 0 ,+maximum-arity+))


(defun* cell-type ((cell heap-cell))
  (:returns heap-cell-tag)
  (logand cell +cell-tag-bitmask+))

(defun* cell-value ((cell heap-cell))
  (:returns heap-cell-value)
  (ash cell (- +cell-tag-width+)))


(defun* cell-type-name ((cell heap-cell))
  (:returns string)
  (eswitch ((cell-type cell) :test #'=)
    (+tag-null+ "NULL")
    (+tag-structure+ "STRUCTURE")
    (+tag-reference+ "REFERENCE")
    (+tag-functor+ "FUNCTOR")))

(defun* cell-type-short-name ((cell heap-cell))
  (:returns string)
  (eswitch ((cell-type cell) :test #'=)
    (+tag-null+ "NUL")
    (+tag-structure+ "STR")
    (+tag-reference+ "REF")
    (+tag-functor+ "FUN")))


(defun* cell-functor-index ((cell heap-cell))
  (:returns functor-index)
  (ash (cell-value cell)
       (- +functor-arity-width+)))

(defun* cell-functor-arity ((cell heap-cell))
  (:returns arity)
  (values
    (logand (cell-value cell)
            +functor-arity-bitmask+)))


(defun* cell-aesthetic ((cell heap-cell))
  "Return a compact, human-friendly string representation of the cell."
  (format nil "[~A~A]"
          (cell-type-short-name cell)
          (eswitch ((cell-type cell))
            (+tag-null+ "")
            (+tag-structure+
              (format nil " ~D" (cell-value cell)))
            (+tag-functor+
              (format nil "functor ~D/~D"
                      (cell-functor-index cell)
                      (cell-functor-arity cell)))
            (+tag-reference+
              (format nil " ~D" (cell-value cell))))))


(defun* make-cell ((tag heap-cell-tag) (value heap-cell-value))
  (:returns heap-cell)
  (values
    (logior (ash value +cell-tag-width+)
            tag)))

(defun* make-cell-null ()
  (:returns heap-cell)
  (make-cell +tag-null+ 0))

(defun* make-cell-structure ((value heap-cell-value))
  (:returns heap-cell)
  (make-cell +tag-structure+ value))

(defun* make-cell-reference ((value heap-cell-value))
  (:returns heap-cell)
  (make-cell +tag-reference+ value))

(defun* make-cell-functor ((functor-index functor-index)
                           (arity arity))
  (:returns heap-cell)
  (make-cell
    +tag-functor+
    (logior (ash functor-index +functor-arity-width+)
            arity)))


;;;; BEHOLD: THE WAM
(defclass wam ()
  ((heap
     :initform (make-array 16
                           :initial-element (make-cell-null)
                           :element-type 'heap-cell)
     :reader wam-heap
     :documentation "The actual heap (stack).")
   (heap-pointer
     :initform 0
     :accessor wam-heap-pointer
     :documentation "The index of the first free cell on the heap (stack).")
   (functors
     :initform (make-array 16
                           :fill-pointer 0
                           :adjustable t
                           :element-type 'symbol)
     :accessor wam-functors
     :documentation "The array of functor symbols in this WAM.")
   (registers
     :reader wam-registers
     :initform (make-array +register-count+
                           :initial-element (make-cell-null)
                           :element-type 'heap-cell)
     :documentation "An array of the X_i registers.")))


(defun make-wam ()
  (make-instance 'wam))


(defun* wam-heap-push! ((wam wam) (cell heap-cell))
  (:returns heap-cell)
  "Push the cell onto the WAM heap and increment the heap pointer.

  Returns the cell.

  "
  (with-slots (heap heap-pointer) wam
    (setf (aref heap heap-pointer) cell)
    (incf heap-pointer)
    cell))

(defun* wam-register ((wam wam) (register register-index))
  (:returns heap-cell)
  "Return the WAM register with the given index."
  (aref (wam-registers wam) register))

(defun (setf wam-register) (new-value wam register)
  (setf (aref (wam-registers wam) register) new-value))


(defun* wam-ensure-functor-index ((wam wam) (functor symbol))
  (:returns functor-index)
  "Return the index of the functor in the WAM's functor table.

  If the functor is not already in the table it will be added.

  "
  (with-slots (functors) wam
    (or (position functor functors)
        (vector-push-extend functor functors))))

(defun* wam-functor-lookup ((wam wam) (functor-index functor-index))
  (:returns symbol)
  "Return the symbol for the functor with the given index in the WAM."
  (aref (wam-functors wam) functor-index))


;;;; Dumping
(defun heap-debug (wam addr cell)
  (switch ((cell-type cell))
    (+tag-reference+
      (if (= addr (cell-value cell))
        "unbound variable"
        (format nil "var pointer to ~D" (cell-value cell))))
    (+tag-functor+
      (format nil "~A/~D"
              (wam-functor-lookup wam (cell-functor-index cell))
              (cell-functor-arity cell)))
    (t "")))

(defun dump-heap (wam from to highlight)
  ;; This code is awful, sorry.
  (let ((heap (wam-heap wam)))
    (format t "HEAP SIZE: ~A~%" (length heap))
    (format t "  +------+-----+--------------+----------------------------+~%")
    (format t "  | ADDR | TYP |        VALUE | DEBUG                      |~%")
    (format t "  +------+-----+--------------+----------------------------+~%")
    (when (> from 0)
      (format t "  |    ⋮ |  ⋮  |            ⋮ |                            |~%"))
    (flet ((print-cell
             (i cell)
             (let ((hi (= i highlight)))
               (format t "~A ~4@A | ~A | ~12@A | ~26A ~A~%"
                       (if hi "==>" "  |")
                       i
                       (cell-type-short-name cell)
                       (cell-value cell)
                       (heap-debug wam i cell)
                       (if hi "<===" "|")))))
      (loop :for i :from from :below to
            :do (print-cell i (aref heap i))))
    (when (< to (length heap))
      (format t "  |    ⋮ |  ⋮  |            ⋮ |                            |~%"))
    (format t "  +------+-----+--------------+----------------------------+~%")
    (values)))


(defun dump-wam-registers (wam)
  (format t "REGISTERS:~%")
  (loop :for i :from 0
        :for reg :across (wam-registers wam)
        :do (format t "~5@A -> ~A~%"
                    (format nil "X~D" i)
                    (cell-aesthetic reg))))

(defun dump-wam (wam from to highlight)
  (dump-wam-registers wam)
  (format t "~%")
  (dump-heap wam from to highlight))

(defun dump-wam-full (wam)
  (dump-wam wam 0 (length (wam-heap wam)) -1))

(defun dump-wam-around (wam addr width)
  (dump-wam wam
            (max 0 (- addr width))
            (min (length (wam-heap wam))
                 (+ addr width 1))
            addr))


;;;; WAM Machine Instructions
(defun* put-structure ((wam wam)
                       (functor symbol)
                       (arity arity)
                       (register register-index))
  (:returns :void)
  (let ((structure-cell (make-cell-structure (1+ (wam-heap-pointer wam))))
        (functor-cell (make-cell-functor
                        (wam-ensure-functor-index wam functor)
                        arity)))
    (wam-heap-push! wam structure-cell)
    (wam-heap-push! wam functor-cell)
    (setf (wam-register wam register) structure-cell))
  (values))

(defun* set-variable ((wam wam) (register register-index))
  (:returns :void)
  (let ((cell (make-cell-reference (wam-heap-pointer wam))))
    (wam-heap-push! wam cell)
    (setf (wam-register wam register) cell))
  (values))

(defun* set-value ((wam wam) (register register-index))
  (:returns :void)
  (wam-heap-push! wam (wam-register wam register))
  (values))


;;;; Terms
(defun parse-term (term)
  "Parse a term into a series of register assignments."
  ;; Turns p(A, q(A, B)) into something like:
  ;;
  ;;   X0 -> p(X1, X2)
  ;;   X1 -> A
  ;;   X2 -> q(X1, X3)
  ;;   X3 -> B
  (labels ((variable-p
             (term)
             (keywordp term))
           (parse-variable
             (var registers)
             ;; If we've already seen this variable, just return its position,
             ;; otherwise allocate a register for it.
             (or (position var registers)
                 (vector-push-extend var registers)))
           (parse-structure
             (structure registers)
             (let* ((functor (first structure))
                    (arguments (rest structure))
                    (contents (list functor)))
               (prog1
                   (vector-push-extend contents registers)
                 ;; Parse the arguments and splice the results into this cell
                 ;; once we're finished.  The children should handle extending
                 ;; the registers as needed.
                 (nconc contents
                        (mapcar #'(lambda (arg)
                                   (parse arg registers))
                                arguments)))))
           (parse (term registers)
                  (if (variable-p term)
                    (parse-variable term registers)
                    (parse-structure term registers))))
    (let ((registers (make-array 64 :fill-pointer 0 :adjustable t)))
      (parse term registers)
      (loop :for i :from 0
            :for reg :across registers
            :collect (cons i reg)))))

(defun flatten-register-assignments (registers)
  "Flatten the set of register assignments into a minimal set."
  ;; Turns:
  ;;
  ;;   X0 -> p(X1, X2)
  ;;   X1 -> A
  ;;   X2 -> q(X1, X3)
  ;;   X3 -> B
  ;;
  ;; into something like:
  ;;
  ;;   X2 -> q(X1, X3), X0 -> p(X1, X2)
  (labels ((variable-assignment-p
             (ass)
             (keywordp (cdr ass)))
           (assignment-less-p
             (ass1 ass2)
             (cond
               ;; If 2 is a variable assignment, nothing can be less than it.
               ((variable-assignment-p ass2) nil)

               ;; If 2 isn't, but 1 is, then 1 < 2.
               ((variable-assignment-p ass1) t)

               ;; Otherwise they're both structure assignments.
               ;; (N . foo A B C)      (M . bar X Y Z)
               ;;
               ;; We need to make sure that if something inside 2 uses the
               ;; target of 1, then 1 < 2.
               ((member (car ass1) (cdr ass2)) t)

               ;; Otherwise we don't care.
               (t nil))))
    (remove-if #'variable-assignment-p
               (sort registers #'assignment-less-p))))

(defun tokenize-assignments (assignments)
  "Tokenize a flattened set of register assignments into a stream."
  ;; Turns:
  ;;
  ;;   X2 -> q(X1, X3), X0 -> p(X1, X2)
  ;;
  ;; into something like:
  ;;
  ;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2
  (mapcan #'(lambda (ass)
             (destructuring-bind (register . (functor . arguments)) ass
               ;; Take a single assignment like:
               ;;   X1 = f(a, b, c)         (1 . (f a b c))
               ;;
               ;; And turn it into a stream of tokens:
               ;;   (X1 = f/3), a, b, c     (1 f 3) a b c
               (cons (list register functor (length arguments))
                     arguments)))
          assignments))

(defun generate-actions (tokens)
  "Generate a series of 'machine instructions' from a stream of tokens."
  ;; Turns:
  ;;
  ;;   (X2 = q/2), X1, X3, (X0 = p/2), X1, X2
  ;;
  ;; into something like:
  ;;
  ;;   (#'put-structure 2 q 2)
  ;;   (#'set-variable 1)
  ;;   (#'set-variable 3)
  ;;   (#'put-structure 0 p 2)
  ;;   (#'set-value 1)
  ;;   (#'set-value 2)
  (let ((seen (list)))
    (flet ((handle-structure
             (register functor arity)
             (push register seen)
             (list #'put-structure functor arity register))
           (handle-register
             (register)
             (if (member register seen)
               (list #'set-value register)
               (progn
                 (push register seen)
                 (list #'set-variable register)))))
      (loop :for token :in tokens
            :collect (if (consp token)
                       (apply #'handle-structure token)
                       (handle-register token))))))


(defun parse (term)
  "Parse a Lisp term into a series of WAM machine instructions."
  (generate-actions
    (tokenize-assignments
      (flatten-register-assignments
        (parse-term term)))))

(defun run (wam instructions)
  "Execute the machine instructions on the given WAM."
  (mapc #'(lambda (action)
            (apply (car action) wam (cdr action)))
        instructions)
  (values))

