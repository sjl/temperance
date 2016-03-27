(in-package #:bones.wam)

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
    (format t "  +------+-----+--------------+----------------------------+~%")
    (format t "  | ADDR | TYP |        VALUE | DEBUG                      |~%")
    (format t "  +------+-----+--------------+----------------------------+~%")
    (when (> from 0)
      (format t "  |    ⋮ |  ⋮  |            ⋮ |                            |~%"))
    (flet ((print-cell (i cell)
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
  (format t  "~5@A ->~4@A~%" "S" (wam-s wam))
  (loop :for i :from 0
        :for reg :across (wam-registers wam)
        :for contents = (wam-register-cell wam i)
        :do (format t "~5@A ->~4@A ~A~%"
                    (format nil "X~D" i)
                    reg
                    (cell-aesthetic contents))))

(defun dump-wam-functors (wam)
  (format t " FUNCTORS: ~S~%" (wam-functors wam)))


(defun dump-wam (wam from to highlight)
  (format t "     FAIL: ~A~%" (wam-fail wam))
  (format t "     MODE: ~A~%" (wam-mode wam))
  (dump-wam-functors wam)
  (format t "HEAP SIZE: ~A~%" (length (wam-heap wam)))
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



(defun extract-thing (wam address)
  (let ((cell (wam-heap-cell wam (deref wam address))))
    (cond
      ((cell-null-p cell)
       "NULL!")
      ((cell-reference-p cell)
       (format nil "var-~D" (cell-value cell)))
      ((cell-structure-p cell)
       (extract-thing wam (cell-value cell)))
      ((cell-functor-p cell)
       (let ((functor (wam-functor-lookup wam (cell-functor-index cell)))
             (arity (cell-functor-arity cell)))
         (list* functor
                (loop :for i :from (1+ address) :to (+ address arity)
                      :collect (extract-thing wam i))))))))
