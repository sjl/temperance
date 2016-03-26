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
    (format t "HEAP SIZE: ~A~%" (length heap))
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

