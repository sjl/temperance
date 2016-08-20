(in-package #:bones.wam)

; (deftype cell-type () ; todo: pick one of these...
;   `(integer 0 ,(1- +number-of-cell-types+)))

(deftype cell-type ()
  'fixnum)

(deftype cell-value ()
  '(or fixnum t))


(deftype type-store ()
  '(simple-array cell-type (*)))

(deftype value-store ()
  '(simple-array cell-value (*)))


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


(deftype fname ()
  'symbol)

(deftype arity ()
  `(integer 0 ,+maximum-arity+))


(deftype code-index ()
  ;; either an address or the sentinel
  `(integer 0 ,(1- +code-limit+)))

(deftype code-word ()
  t)


(deftype generic-code-store ()
  `(simple-array code-word (*)))

(deftype query-code-holder ()
  `(simple-array code-word (,+maximum-query-size+)))

(deftype query-size ()
  `(integer 0 ,+maximum-query-size+))

(deftype instruction-size ()
  `(integer 1 ,+maximum-instruction-size+))


(deftype opcode ()
  `(integer 0 ,(1- +number-of-opcodes+)))


(deftype stack-frame-size ()
  `(integer 4 ,+stack-frame-size-limit+))

(deftype stack-choice-size ()
  ;; TODO: is this actually right?  check on frame size limit vs choice point
  ;; size limit...
  `(integer 8 ,+stack-frame-size-limit+))

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
    stack-frame-argcount)) ; N

(deftype stack-choice-word ()
  '(or
    environment-pointer ; CE
    backtrack-pointer ; B, CC
    continuation-pointer ; CP, BP
    stack-frame-argcount ; N
    trail-index ; TR
    heap-index)) ; H

(deftype stack-word ()
  '(or stack-frame-word stack-choice-word))


;;;; Sanity Checks
;;; The values on the WAM stack are a bit of a messy situation.  The WAM store
;;; is defined as an array of cells, but certain things on the stack aren't
;;; actually cells (e.g. the stored continuation pointer).
;;;
;;; This shouldn't be a problem (aside from being ugly) as long as they all fit
;;; inside fixnums... so let's just make sure that's the case.

(defun sanity-check-stack-type (type)
  (assert (subtypep type 'fixnum) ()
    "Type ~A is too large!"
    type)
  (values))

(sanity-check-stack-type 'stack-frame-argcount)
(sanity-check-stack-type 'environment-pointer)
(sanity-check-stack-type 'continuation-pointer)
(sanity-check-stack-type 'backtrack-pointer)
(sanity-check-stack-type 'trail-index)
(sanity-check-stack-type 'stack-word)
