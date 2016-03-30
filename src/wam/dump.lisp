(in-package #:bones.wam)

(defun registers-pointing-to (wam addr)
  (loop :for reg :across (wam-registers wam)
        :for i :from 0
        :when (= reg addr)
        :collect i))

(defun heap-debug (wam addr cell)
  (format
    nil "~A~{(X~A) ~}"
    (switch ((cell-type cell))
      (+tag-reference+
        (if (= addr (cell-value cell))
          "unbound variable "
          (format nil "var pointer to ~D " (cell-value cell))))
      (+tag-functor+
        (format nil "~A/~D "
                (wam-functor-lookup wam (cell-functor-index cell))
                (cell-functor-arity cell)))
      (t ""))
    (registers-pointing-to wam addr)))

(defun dump-heap (wam from to highlight)
  ;; This code is awful, sorry.
  (let ((heap (wam-heap wam)))
    (format t "HEAP~%")
    (format t "  +------+-----+--------------+--------------------------------------+~%")
    (format t "  | ADDR | TYP |        VALUE | DEBUG                                |~%")
    (format t "  +------+-----+--------------+--------------------------------------+~%")
    (when (> from 0)
      (format t "  |    ⋮ |  ⋮  |            ⋮ |                                      |~%"))
    (flet ((print-cell (i cell)
             (let ((hi (= i highlight)))
               (format t "~A ~4@A | ~A | ~12@A | ~36A ~A~%"
                       (if hi "==>" "  |")
                       i
                       (cell-type-short-name cell)
                       (cell-value cell)
                       (heap-debug wam i cell)
                       (if hi "<===" "|")))))
      (loop :for i :from from :below to
            :do (print-cell i (aref heap i))))
    (when (< to (length heap))
      (format t "  |    ⋮ |  ⋮  |            ⋮ |                                      |~%"))
    (format t "  +------+-----+--------------+--------------------------------------+~%")
    (values)))


(defun instruction-aesthetic (instruction)
  (format nil "~A~{ ~4,'0X~}"
          (opcode-short-name (aref instruction 0))
          (rest (coerce instruction 'list))))

(defun dump-code (wam)
  (let ((code (wam-code wam)))
    (format t "CODE~%")
    (let ((addr 0))
      (while (< addr (length code))
        (format t "; ~4,'0X: " addr)
        (let ((instruction (wam-code-instruction wam addr)))
          (format t "~A~%" (instruction-aesthetic instruction))
          (incf addr (length instruction)))))))


(defun dump-wam-registers (wam)
  (format t "REGISTERS:~%")
  (format t  "~5@A ->~6@A~%" "S" (wam-s wam))
  (loop :for i :from 0
        :for reg :across (wam-registers wam)
        :for contents = (when (not (= reg (1- +heap-limit+)))
                          (wam-register-cell wam i))
        :do (format t "~5@A ->~6@A ~A~%"
                    (format nil "X~D" i)
                    reg
                    (if contents
                      (cell-aesthetic contents)
                      "unset"))))

(defun dump-wam-functors (wam)
  (format t " FUNCTORS: ~S~%" (wam-functors wam)))


(defun dump-wam (wam from to highlight)
  (format t "     FAIL: ~A~%" (wam-fail wam))
  (format t "     MODE: ~A~%" (wam-mode wam))
  (dump-wam-functors wam)
  (format t "HEAP SIZE: ~A~%" (length (wam-heap wam)))
  (format t "PROGRAM C: ~A~%" (wam-program-counter wam))
  (dump-wam-registers wam)
  (format t "~%")
  (dump-heap wam from to highlight)
  (format t "~%")
  (dump-code wam))

(defun dump-wam-full (wam)
  (dump-wam wam 0 (length (wam-heap wam)) -1))

(defun dump-wam-around (wam addr width)
  (dump-wam wam
            (max 0 (- addr width))
            (min (length (wam-heap wam))
                 (+ addr width 1))
            addr))


(defun extract-thing (wam &optional (address (wam-register wam 0)))
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
                      :collect (extract-thing wam i)))))
      (t (error "What to heck is this?")))))
