(in-package #:bones.wam)

(deftype heap-cell ()
  `(unsigned-byte ,+cell-width+))

(deftype heap-cell-tag ()
  `(unsigned-byte ,+cell-tag-width+))

(deftype heap-cell-value ()
  `(unsigned-byte ,+cell-value-width+))


(deftype heap-index ()
  `(integer 0 ,(1- +heap-limit+)))

(deftype stack-index ()
  `(integer 0 ,(1- +stack-limit+)))

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
  '(integer 0 23))


(deftype stack-frame-size ()
  `(integer 3 ,+stack-frame-size-limit+))

(deftype stack-frame-argcount ()
  `(integer 0 ,+register-count+))

(deftype continuation-pointer ()
  'code-index)

(deftype environment-pointer ()
  'stack-index)

(deftype stack-word ()
  '(or
    environment-pointer ; CE
    continuation-pointer ; CP
    stack-frame-argcount ; N
    heap-index)) ; YN
