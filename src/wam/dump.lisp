(in-package #:bones.wam)

(defun registers-pointing-to (wam addr)
  (loop :for reg :across (wam-local-registers wam)
        :for i :from 0
        :when (= reg addr)
        :collect i))

(defun heap-debug (wam addr cell indent-p)
  (format
    nil "~A~A~{<-X~A ~}"
    (if indent-p
      "  "
      "")
    (switch ((cell-type cell))
      (+tag-reference+
        (if (= addr (cell-value cell))
          "unbound variable "
          (format nil "var pointer to ~4,'0X " (cell-value cell))))
      (+tag-structure+
        (format nil "structure pointer to ~4,'0X " (cell-value cell)))
      (+tag-functor+
        (destructuring-bind (functor . arity)
            (wam-functor-lookup wam (cell-functor-index cell))
          (format nil "~A/~D " functor arity)))
      (t ""))
    (registers-pointing-to wam addr)))

(defun dump-heap (wam from to highlight)
  ;; This code is awful, sorry.
  (let ((heap (wam-heap wam)))
    (format t "HEAP~%")
    (format t "  +------+-----+----------+--------------------------------------+~%")
    (format t "  | ADDR | TYP |    VALUE | DEBUG                                |~%")
    (format t "  +------+-----+----------+--------------------------------------+~%")
    (when (> from 0)
      (format t "  |    ⋮ |  ⋮  |        ⋮ |                                      |~%"))
    (flet ((print-cell (i cell indent)
             (let ((hi (= i highlight)))
               (format t "~A ~4,'0X | ~A | ~8,'0X | ~36A ~A~%"
                       (if hi "==>" "  |")
                       i
                       (cell-type-short-name cell)
                       (cell-value cell)
                       (heap-debug wam i cell (> indent 0))
                       (if hi "<===" "|")))))
      (loop :for i :from from :below to
            :with indent = 0
            :for cell = (aref heap i)
            :do
            (progn
              (print-cell i cell indent)
              (if (cell-functor-p cell)
                (setf indent (wam-functor-arity wam (cell-functor-index cell)))
                (when (not (zerop indent))
                  (decf indent))))))
    (when (< to (length heap))
      (format t "  |    ⋮ |  ⋮  |        ⋮ |                                      |~%"))
    (format t "  +------+-----+----------+--------------------------------------+~%")
    (values)))


(defun dump-stack (wam &optional (e (wam-environment-pointer wam)))
  (format t "STACK~%")
  (format t "  +------+----------+-------------------------------+~%")
  (format t "  | ADDR |    VALUE |                               |~%")
  (format t "  +------+----------+-------------------------------+~%")
  (loop :with n = nil
        :with arg = 0
        :for offset = 0 :then (1+ offset)
        :for cell :across (wam-stack wam)
        :for addr :from 0 :do
        (format t "  | ~4,'0X | ~8,'0X | ~30A|~A~A~%"
                addr
                cell
                (cond
                  ((= offset 0) "CE ===========================")
                  ((= offset 1) "CP")
                  ((= offset 2)
                   (if (zerop cell)
                     (progn
                       (setf offset -1)
                       "N: EMPTY")
                     (progn
                       (setf n cell)
                       (format nil "N: ~D" cell))))
                  ((< arg n)
                   (prog1
                       (format nil " Y~D: ~4,'0X"
                               arg
                               ;; look up the actual cell in the heap
                               (cell-aesthetic (wam-heap-cell wam cell)))
                     (when (= n (incf arg))
                       (setf offset -1
                             n nil
                             arg 0)))))
                (if (= addr (wam-environment-pointer wam)) " <- E" "")
                (if (= addr e) " <- FRAME" "")))
  (format t "  +------+----------+-------------------------------+~%"))


(defun pretty-functor (functor-index functor-list)
  (when functor-list
    (destructuring-bind (symbol . arity)
        (elt functor-list functor-index)
      (format nil "~A/~D" symbol arity))))

(defun pretty-arguments (arguments)
  (format nil "~{ ~4,'0X~}" arguments))


(defgeneric instruction-details (opcode arguments functor-list))

(defmethod instruction-details ((opcode t) arguments functor-list)
  (format nil "~A~A"
          (opcode-short-name opcode)
          (pretty-arguments arguments)))


(defmethod instruction-details ((opcode (eql +opcode-set-variable-local+)) arguments functor-list)
  (format nil "SVAR~A      ; X~A <- new unbound REF"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-set-variable-stack+)) arguments functor-list)
  (format nil "SVAR~A      ; Y~A <- new unbound REF"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-set-value-local+)) arguments functor-list)
  (format nil "SVLU~A      ; new REF to X~A"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-set-value-stack+)) arguments functor-list)
  (format nil "SVLU~A      ; new REF to Y~A"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-structure-local+)) arguments functor-list)
  (format nil "GETS~A ; X~A = ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments) functor-list)))

(defmethod instruction-details ((opcode (eql +opcode-put-structure-local+)) arguments functor-list)
  (format nil "PUTS~A ; X~A <- new ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments) functor-list)))

(defmethod instruction-details ((opcode (eql +opcode-get-variable-local+)) arguments functor-list)
  (format nil "GVAR~A ; X~A <- A~A"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-variable-stack+)) arguments functor-list)
  (format nil "GVAR~A ; Y~A <- A~A"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-value-local+)) arguments functor-list)
  (format nil "GVLU~A ; X~A = A~A"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-value-stack+)) arguments functor-list)
  (format nil "GVLU~A ; Y~A = A~A"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-variable-local+)) arguments functor-list)
  (format nil "PVAR~A ; X~A <- A~A <- new unbound REF"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-variable-stack+)) arguments functor-list)
  (format nil "PVAR~A ; Y~A <- A~A <- new unbound REF"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-value-local+)) arguments functor-list)
  (format nil "PVLU~A ; A~A <- X~A"
          (pretty-arguments arguments)
          (second arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-value-stack+)) arguments functor-list)
  (format nil "PVLU~A ; A~A <- Y~A"
          (pretty-arguments arguments)
          (second arguments)
          (first arguments)))


(defmethod instruction-details ((opcode (eql +opcode-call+)) arguments functor-list)
  (format nil "CALL~A      ; ~A"
          (pretty-arguments arguments)
          (pretty-functor (first arguments) functor-list)))


(defun dump-code-store (wam code-store
                        &optional
                        (from 0)
                        (to (length code-store)))
  ;; This is a little trickier than might be expected.  We have to walk from
  ;; address 0 no matter what `from` we get, because instruction sizes vary and
  ;; aren't aligned.  So if we just start at `from` we might start in the middle
  ;; of an instruction and everything would be fucked.
  (let ((addr 0)
        (lbls (bones.utils::invert-hash-table (wam-code-labels wam)))) ; oh god
    (while (< addr to)
      (let ((instruction (retrieve-instruction code-store addr)))
        (when (>= addr from)
          (let ((lbl (gethash addr lbls))) ; forgive me
            (when lbl
              (format t ";;;; BEGIN ~A~%"
                      (pretty-functor lbl (wam-functors wam)))))
          (format t ";~A~4,'0X: "
                  (if (= (wam-program-counter wam) addr)
                    ">>"
                    "  ")
                  addr)
          (format t "~A~%" (instruction-details (aref instruction 0)
                                                (rest (coerce instruction 'list))
                                                (wam-functors wam))))
        (incf addr (length instruction))))))

(defun dump-code
    (wam
     &optional
     (from (max (- (wam-program-counter wam) 8) ; wow
                0)) ; this
     (to (min (+ (wam-program-counter wam) 12) ; is
              (length (wam-code wam))))) ; bad
  (format t "CODE~%")
  (dump-code-store wam (wam-code wam) from to))


(defun dump-wam-registers (wam)
  (format t "REGISTERS:~%")
  (format t  "~5@A ->~6@A~%" "S" (wam-s wam))
  (loop :for i :from 0
        :for reg :across (wam-local-registers wam)
        :for contents = (when (not (= reg (1- +heap-limit+)))
                          (wam-heap-cell wam reg))
        :when contents
        :do (format t "~5@A ->~6@A ~10A ~A~%"
                    (format nil "X~D" i)
                    reg
                    (cell-aesthetic contents)
                    (format nil "; ~A" (first (extract-things wam (list reg)))))))

(defun dump-wam-functors (wam)
  (format t " FUNCTORS: ~S~%" (wam-functors wam)))

(defun dump-labels (wam)
  (format t "LABELS:~%~{  ~A -> ~4,'0X~^~%~}~%"
          (loop :for functor-index
                :being :the :hash-keys :of (wam-code-labels wam)
                :using (hash-value address)
                :nconc (list (pretty-functor functor-index
                                             (wam-functors wam))
                             address))))


(defun dump-wam (wam from to highlight)
  (format t "     FAIL: ~A~%" (wam-fail wam))
  (format t "     MODE: ~S~%" (wam-mode wam))
  (dump-wam-functors wam)
  (format t "HEAP SIZE: ~A~%" (length (wam-heap wam)))
  (format t "PROGRAM C: ~A~%" (wam-program-counter wam))
  (format t "CONT  PTR: ~A~%" (wam-continuation-pointer wam))
  (format t "ENVIR PTR: ~A~%" (wam-environment-pointer wam))
  (dump-wam-registers wam)
  (format t "~%")
  (dump-heap wam from to highlight)
  (format t "~%")
  (dump-stack wam)
  (format t "~%")
  (dump-labels wam)
  (dump-code wam))

(defun dump-wam-full (wam)
  (dump-wam wam 0 (length (wam-heap wam)) -1))

(defun dump-wam-around (wam addr width)
  (dump-wam wam
            (max 0 (- addr width))
            (min (length (wam-heap wam))
                 (+ addr width 1))
            addr))


