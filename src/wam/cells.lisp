(in-package #:bones.wam)

;;; The cells of the WAM are essentially N bit bytes, with different chunks of
;;; bits representing different things.  All cells have type tag bits in the
;;; low-order bits and their value in the higher-order bits:
;;;
;;;   value         type
;;;   vvvvvvvvvvvvvTTT
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
;;; FUNCTOR cell values are an index into the WAM's functor array where the
;;; `(symbol . arity)` cons lives.
;;;
;;; CONSTANT cells are the same as functor cells, except that they always refer
;;; to functors with an arity of zero.
;;;


(declaim (inline cell-type
                 cell-value))
(defun* cell-type ((cell cell))
  (:returns cell-tag)
  (logand cell +cell-tag-bitmask+))

(defun* cell-value ((cell cell))
  (:returns cell-value)
  (ash cell (- +cell-tag-width+)))


(defun* cell-type-name ((cell cell))
  (:returns string)
  (eswitch ((cell-type cell) :test #'=)
    (+tag-null+ "NULL")
    (+tag-structure+ "STRUCTURE")
    (+tag-reference+ "REFERENCE")
    (+tag-functor+ "FUNCTOR")
    (+tag-constant+ "CONSTANT")))

(defun* cell-type-short-name ((cell cell))
  (:returns string)
  (eswitch ((cell-type cell) :test #'=)
    (+tag-null+ "NUL")
    (+tag-structure+ "STR")
    (+tag-reference+ "REF")
    (+tag-functor+ "FUN")
    (+tag-constant+ "CON")))


(defun* cell-aesthetic ((cell cell))
  "Return a compact, human-friendly string representation of the cell."
  (format nil "[~A ~X]"
          (cell-type-short-name cell)
          (cell-value cell)))


(declaim (inline cell-null-p
                 cell-reference-p
                 cell-functor-p
                 cell-structure-p
                 cell-constant-p
                 cell-list-p))
(defun* cell-null-p ((cell cell))
  (:returns boolean)
  (= (cell-type cell) +tag-null+))

(defun* cell-reference-p ((cell cell))
  (:returns boolean)
  (= (cell-type cell) +tag-reference+))

(defun* cell-functor-p ((cell cell))
  (:returns boolean)
  (= (cell-type cell) +tag-functor+))

(defun* cell-structure-p ((cell cell))
  (:returns boolean)
  (= (cell-type cell) +tag-structure+))

(defun* cell-constant-p ((cell cell))
  (:returns boolean)
  (= (cell-type cell) +tag-constant+))

(defun* cell-list-p ((cell cell))
  (:returns boolean)
  (= (cell-type cell) +tag-list+))


(declaim (inline make-cell
                 make-cell-null
                 make-cell-structure
                 make-cell-reference
                 make-cell-functor
                 make-cell-constant
                 make-cell-list))
(defun* make-cell ((tag cell-tag) (value cell-value))
  (:returns cell)
  (values
    (logior (ash value +cell-tag-width+)
            tag)))

(defun* make-cell-null ()
  (:returns cell)
  (make-cell +tag-null+ 0))

(defun* make-cell-structure ((value cell-value))
  (:returns cell)
  (make-cell +tag-structure+ value))

(defun* make-cell-reference ((value cell-value))
  (:returns cell)
  (make-cell +tag-reference+ value))

(defun* make-cell-functor ((functor-index functor-index))
  (:returns cell)
  (make-cell +tag-functor+ functor-index))

(defun* make-cell-constant ((functor-index functor-index))
  (:returns cell)
  (make-cell +tag-constant+ functor-index))

(defun* make-cell-list ((value cell-value))
  (:returns cell)
  (make-cell +tag-list+ value))


