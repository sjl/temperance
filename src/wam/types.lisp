(in-package #:bones.wam)

(deftype heap-cell ()
  `(unsigned-byte ,+cell-width+))

(deftype heap-cell-tag ()
  `(unsigned-byte ,+cell-tag-width+))

(deftype heap-cell-value ()
  `(unsigned-byte ,+cell-value-width+))


(deftype heap-index ()
  `(integer 0 ,(1- +heap-limit+)))

(deftype register-index ()
  `(integer 0 ,(1- +register-count+)))

(deftype functor-index ()
  `(integer 0 ,(1- array-total-size-limit)))


(deftype arity ()
  `(integer 0 ,+maximum-arity+))

(deftype functor ()
  '(cons symbol arity))


(deftype code-word ()
  `(unsigned-byte ,+code-word-size+))

(deftype code-index ()
  ; either an address or the sentinal
  `(integer 0 ,(1- +code-limit+)))

(deftype opcode ()
  '(integer 0 12))
