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


(defmethod instruction-details ((opcode (eql +opcode-set-variable+)) arguments functor-list)
  (format nil "SVAR~A      ; ~A <- new unbound REF"
          (pretty-arguments arguments)
          (register-designator-to-string (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-set-value+)) arguments functor-list)
  (format nil "SVLU~A      ; new REF to ~A"
          (pretty-arguments arguments)
          (register-designator-to-string (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-get-structure+)) arguments functor-list)
  (format nil "GETS~A ; ~A = ~A"
          (pretty-arguments arguments)
          (register-designator-to-string (second arguments))
          (pretty-functor (first arguments) functor-list)))

(defmethod instruction-details ((opcode (eql +opcode-put-structure+)) arguments functor-list)
  (format nil "PUTS~A ; ~A <- new ~A"
          (pretty-arguments arguments)
          (register-designator-to-string (second arguments))
          (pretty-functor (first arguments) functor-list)))


(defmethod instruction-details ((opcode (eql +opcode-get-variable+)) arguments functor-list)
  (format nil "GVAR~A ; ~A <- ~A"
          (pretty-arguments arguments)
          (register-designator-to-string (first arguments))
          (register-designator-to-string (second arguments))))

(defmethod instruction-details ((opcode (eql +opcode-get-value+)) arguments functor-list)
  (format nil "GVLU~A ; ~A = ~A"
          (pretty-arguments arguments)
          (register-designator-to-string (second arguments))
          (register-designator-to-string (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-put-variable+)) arguments functor-list)
  (format nil "PVAR~A ; ~A <- ~A <- new unbound REF"
          (pretty-arguments arguments)
          (register-designator-to-string (second arguments))
          (register-designator-to-string (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-put-value+)) arguments functor-list)
  (format nil "PVLU~A ; ~A <- ~A"
          (pretty-arguments arguments)
          (register-designator-to-string (second arguments))
          (register-designator-to-string (first arguments))))


(defmethod instruction-details ((opcode (eql +opcode-call+)) arguments functor-list)
  (format nil "CALL~A      ; ~A"
          (pretty-arguments arguments)
          (pretty-functor (first arguments) functor-list)))


(defun dump-code-store (wam code-store
                            &optional
                            (from 0)
                            (to (length code-store)))
  (let ((addr from)
        (lbls (bones.utils::invert-hash-table (wam-code-labels wam)))) ; oh god
    (while (< addr to)
      (let ((lbl (gethash addr lbls))) ; forgive me
        (when lbl
          (format t ";;;; BEGIN ~A~%"
                  (pretty-functor lbl (wam-functors wam)))))
      (format t "; ~4,'0X: " addr)
      (let ((instruction (retrieve-instruction code-store addr)))
        (format t "~A~%" (instruction-details (aref instruction 0)
                                              (rest (coerce instruction 'list))
                                              (wam-functors wam)))
        (incf addr (length instruction))))))

(defun dump-code (wam &optional (from 0) (to (length (wam-code wam))))
  (format t "CODE~%")
  (dump-code-store wam (wam-code wam) from to))


(defun extract-thing (wam address)
  "Extract the thing at the given heap address."
  (let ((cell (wam-heap-cell wam (deref wam address))))
    (cond
      ((cell-null-p cell)
       "NULL!")
      ((cell-reference-p cell)
       ;; TODO: figure out what the hell to return here
       (gensym (format nil "var@~4,'0X-" (cell-value cell))))
      ((cell-structure-p cell)
       (extract-thing wam (cell-value cell)))
      ((cell-functor-p cell)
       (destructuring-bind (functor . arity)
           (wam-functor-lookup wam (cell-functor-index cell))
         (list* functor
                (loop :for i :from (1+ address) :to (+ address arity)
                      :collect (extract-thing wam i)))))
      (t (error "What to heck is this?")))))


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
                    (format nil "; ~A" (extract-thing wam reg)))))

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


