(in-package #:bones.wam)

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
              (format nil " functor ~D/~D"
                      (cell-functor-index cell)
                      (cell-functor-arity cell)))
            (+tag-reference+
              (format nil " ~D" (cell-value cell))))))


(defun* cell-null-p ((cell heap-cell))
  (:returns boolean)
  (= (cell-type cell) +tag-null+))

(defun* cell-reference-p ((cell heap-cell))
  (:returns boolean)
  (= (cell-type cell) +tag-reference+))

(defun* cell-functor-p ((cell heap-cell))
  (:returns boolean)
  (= (cell-type cell) +tag-functor+))

(defun* cell-structure-p ((cell heap-cell))
  (:returns boolean)
  (= (cell-type cell) +tag-structure+))


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


