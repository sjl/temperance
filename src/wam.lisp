(in-package #:bones.wam)

(declaim (optimize (safety 3) (debug 3)))

;;;; Utilities
(defun pb (b)
  (format t "~B~%" b))


;;;; Heap Cells
(define-constant +cell-width+ 16
  :documentation "Number of bits in each heap cell.")

(define-constant +cell-tag-width+ 2
  :documentation "Number of bits reserved for cell type tags.")

(define-constant +cell-value-width+ (- +cell-width+ +cell-tag-width+)
  :documentation "Number of bits reserved for cell values.")

(define-constant +cell-tag-bitmask+ (1- (ash 1 +cell-tag-width+))
  :documentation "Bitmask for masking the cell type tags.")

(define-constant +tag-null+      #b00
  :documentation "An empty cell.")

(define-constant +tag-structure+ #b01
  :documentation "A structure cell.")

(define-constant +tag-reference+ #b10
  :documentation "A pointer to a cell.")

(define-constant +tag-functor+   #b11
  :documentation "A functor.")


(defparameter functor-arity-width 4
  "Number of bits dedicated to functor arity.")

(defparameter functor-arity-bitmask #b1111
  "Bitmask for the functor arity bits.")


(deftype heap-cell ()
  `(unsigned-byte ,+cell-width+))

(deftype heap-cell-tag ()
  `(unsigned-byte ,+cell-tag-width+))

(deftype heap-cell-value ()
  `(unsigned-byte ,+cell-value-width+))


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


(defun* make-cell ((tag heap-cell-tag) (value heap-cell-value))
  (:returns heap-cell)
  (logior (ash value +cell-tag-width+)
          tag))

(defun* make-cell-null ()
  (:returns heap-cell)
  (make-cell +tag-null+ 0))

(defun* make-cell-structure ((value heap-cell-value))
  (:returns heap-cell)
  (make-cell +tag-structure+ value))

(defun* make-cell-reference ((value heap-cell-value))
  (:returns heap-cell)
  (make-cell +tag-reference+ value))

(defun* make-cell-functor ((functor symbol) (arity (integer 0)))
  (:returns heap-cell)
  (make-cell +tag-functor+ arity))


; (defun cell-functor-name)
(defun cell-functor-arity (cell)
  (logand (cell-value cell)
          functor-arity-bitmask))


;;;; Heap
(deftype heap-index ()
  `(integer 0 ,array-total-size-limit))


(defun heap-debug (addr cell)
  (cond
    ((= +tag-reference+ (cell-type cell))
     (if (= addr (cell-value cell))
       "unbound variable"
       "variable pointer"))
    ((= +tag-functor+ (cell-type cell))
     (format nil "functor/~D" (cell-functor-arity cell)))
    (t "")))

(defun dump-heap (heap from to highlight)
  (format t "~%Dumping heap...~%")
  (format t "Heap size: ~A~%~%" (length heap))
  (format t "+------+-----+--------------+----------------------------+~%")
  (format t "| ADDR | TYP |        VALUE | DEBUG                      |~%")
  (format t "+------+-----+--------------+----------------------------+~%")
  (flet ((print-cell
           (i cell)
           (format t "| ~4@A | ~A | ~12@A | ~26A |~A~%"
                   i
                   (cell-type-short-name cell)
                   (cell-value cell)
                   (heap-debug i cell)
                   (if (= i highlight) " <===" ""))))
    (loop :for i :from from :below to
          :do (print-cell i (aref heap i))))
  (format t "+------+-----+--------------+----------------------------+~%")
  (values))

(defun dump-heap-full (heap)
  (dump-heap heap 0 (length heap) -1))

(defun dump-heap-around (heap addr width)
  (dump-heap heap
             (max 0 (- addr width))
             (min (length heap) (+ addr width 1))
             addr))


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
   (registers
     :reader wam-registers
     :initform (make-array 16
                           :initial-element (make-cell-null)
                           :element-type 'heap-cell)
     :documentation "An array of the X_i registers.")))

(defun make-wam ()
  (make-instance 'wam))


;;;; Terms
(defparameter p
  '(p :z
      (h :z :w)
      (f :w)))


(defun parse-term (term)
  "Parse a term into a series of register assignments."
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

(defun dump-parse (term)
  (loop :for (i . reg) :in (parse-term term)
        :do (format t "X~A -> ~S~%" i reg)))


(defun flatten-register-assignments (registers)
  "Flatten the set of register assignments into a minimal set."
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


(defun* put-structure ((wam wam)
                       functor
                       (arity (integer 0))
                       (register (integer 0)))
  (with-slots (heap registers heap-pointer) wam
    (setf (aref heap heap-pointer)
          (make-cell-structure (1+ heap-pointer)))
    (setf (aref heap (1+ heap-pointer))
          (make-cell-functor functor arity))
    (setf (aref registers register)
          (aref heap heap-pointer))
    (incf heap-pointer 2)))

(defun* set-variable ((wam wam) (register (integer 0)))
  (with-slots (heap registers heap-pointer) wam
    ;; This cell will reference itself (i.e. it's an unbound variable).
    (setf (aref heap heap-pointer)
          (make-cell-reference heap-pointer))
    ;; Set the register to match the cell we just made.
    (setf (aref registers register)
          (aref heap heap-pointer))
    ;; Bump the heap pointer.
    (incf heap-pointer)))

(defun* set-value ((wam wam) (register (integer 0)))
  (with-slots (heap registers heap-pointer) wam
    (setf (aref heap heap-pointer)
          (aref registers register))
    (incf heap-pointer)))


(defun generate-actions (tokens)
  "Generate a series of 'machine instructions' from a stream of tokens."
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


(defun build-heap (wam actions)
  (mapc #'(lambda (action)
            (apply (car action) wam (cdr action)))
        actions)
  (values))


; (defparameter *wam* (make-wam))

; (dump-heap-full (wam-heap *wam*))

; (build-heap
;   *wam*
;   (generate-actions
;     (tokenize-assignments
;       (flatten-register-assignments
;         (parse-term p)))))

; (dump-heap-full (wam-heap *wam*))

