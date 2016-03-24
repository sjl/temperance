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

(define-constant +tag-symbol+    #b11
  :documentation "A constant symbol.")


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
  (ash cell (- +cell-tag-bit-length+)))


(defun* cell-type-name ((cell heap-cell))
  (:returns string)
  (eswitch ((cell-type cell) :test #'=)
    (+tag-null+ "NULL")
    (+tag-structure+ "STRUCTURE")
    (+tag-reference+ "REFERENCE")
    (+tag-symbol+ "SYMBOL")))

(defun* cell-type-short-name ((cell heap-cell))
  (:returns string)
  (eswitch ((cell-type cell) :test #'=)
    (+tag-null+ "NUL")
    (+tag-structure+ "STR")
    (+tag-reference+ "REF")
    (+tag-symbol+ "SYM")))


(defun* make-cell ((tag heap-cell-tag) (value heap-cell-value))
  (:returns heap-cell)
  (logior (ash value +cell-tag-bit-length+)
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

(defun* make-cell-symbol ((value heap-cell-value))
  (:returns heap-cell)
  (make-cell +tag-symbol+ value))


;;;; Heap
(deftype heap-index ()
  `(integer 0 ,array-total-size-limit))

(defparameter *heap*
  (make-array 16
              :initial-element (make-cell-null)
              :element-type 'heap-cell))

(defun dump-heap (heap from to highlight)
  (format t "~%Dumping heap...~%")
  (format t "Heap size: ~A~%~%" (length heap))
  (format t "+------+-----+--------------+~%")
  (format t "| ADDR | TYP |        VALUE |~%")
  (format t "+------+-----+--------------+~%")
  (flet ((print-cell
           (i cell)
           (format t "| ~4@A | ~A | ~12@A |~A~%"
                   i
                   (cell-type-short-name cell)
                   (cell-value cell)
                   (if (= i highlight) " <===" ""))))
    (loop :for i :from from :below to
          :do (print-cell i (aref heap i))))
  (format t "+------+-----+--------------+~%")
  (values))

(defun dump-heap-full (heap)
  (dump-heap heap 0 (length heap) -1))

(defun dump-heap-around (heap addr width)
  (dump-heap heap
             (max 0 (- addr width))
             (min (length heap) (+ addr width 1))
             addr))


(setf (aref *heap* 0) (make-cell-structure 12))
(setf (aref *heap* 1) (make-cell-reference 42))
(setf (aref *heap* 2) (make-cell-symbol 112))

(dump-heap-full *heap*)


;;;; Terms
(defparameter p
  '(p :z
      (h :z :w)
      (f :w)))

