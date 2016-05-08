(in-package #:bones.wam)

(deftype heap-cell ()
  `(unsigned-byte ,+cell-width+))

(deftype heap-cell-tag ()
  `(unsigned-byte ,+cell-tag-width+))

(deftype heap-cell-value ()
  `(unsigned-byte ,+cell-value-width+))


(deftype store-index ()
  `(integer 0 ,(1- +store-limit+)))

(deftype heap-index ()
  `(integer ,+heap-start+ ,(1- +store-limit+)))

(deftype stack-index ()
  `(integer ,+stack-start+ ,(1- +stack-end+)))

(deftype trail-index ()
  `(integer 0 ,(1- +trail-limit+)))

(deftype register-index ()
  `(integer 0 ,(1- +register-count+)))

(deftype functor-index ()
  `(integer 0 ,(1- +functor-limit+)))


(deftype arity ()
  `(integer 0 ,+maximum-arity+))

(deftype functor ()
  '(cons symbol arity))


(deftype code-word ()
  `(unsigned-byte ,+code-word-size+))

(deftype code-index ()
  ;; either an address or the sentinal
  `(integer 0 ,(1- +code-limit+)))


(deftype opcode ()
  '(integer 0 26))


(deftype stack-frame-size ()
  `(integer 3 ,+stack-frame-size-limit+))

(deftype stack-choice-size ()
  `(integer 7 ,+stack-frame-size-limit+))

(deftype stack-frame-argcount ()
  'arity)

(deftype continuation-pointer ()
  'code-index)

(deftype environment-pointer ()
  'stack-index)

(deftype backtrack-pointer ()
  'stack-index)


(deftype stack-frame-word ()
  '(or
    environment-pointer ; CE
    continuation-pointer ; CP
    stack-frame-argcount ; N
    heap-index)) ; Yn

(deftype stack-choice-word ()
  '(or
    environment-pointer ; CE
    backtrack-pointer ; B
    continuation-pointer ; CP, BP
    stack-frame-argcount ; N
    trail-index ; TR
    heap-index))  ; An, H

(deftype stack-word ()
  '(or stack-frame-word stack-choice-word))


