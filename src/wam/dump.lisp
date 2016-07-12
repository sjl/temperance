(in-package #:bones.wam)

(defun registers-pointing-to (wam addr)
  (loop
    :for r :from 0 :below +register-count+
    :for reg = (wam-local-register wam r)
    :when (= reg addr)
    :collect r))

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
            (wam-functor-lookup wam (cell-value cell))
          (format nil "~A/~D " functor arity)))
      (+tag-constant+
        (format nil "~A/0 " (wam-functor-symbol wam (cell-value cell))))
      (t ""))
    (registers-pointing-to wam addr)))

(defun dump-heap (wam from to highlight)
  ;; This code is awful, sorry.
  (let ((store (wam-store wam)))
    (format t "HEAP~%")
    (format t "  +------+-----+------------------+--------------------------------------+~%")
    (format t "  | ADDR | TYP |            VALUE | DEBUG                                |~%")
    (format t "  +------+-----+------------------+--------------------------------------+~%")
    (when (> from +heap-start+)
      (format t "  |    ⋮ |  ⋮  |                ⋮ |                                      |~%"))
    (flet ((print-cell (i cell indent)
             (let ((hi (= i highlight)))
               (format t "~A ~4,'0X | ~A | ~16,'0X | ~36A ~A~%"
                       (if hi "==>" "  |")
                       i
                       (cell-type-short-name cell)
                       (cell-value cell)
                       (heap-debug wam i cell (> indent 0))
                       (if hi "<===" "|")))))
      (loop :for i :from from :below to
            :with indent = 0
            :for cell = (aref store i)
            :do
            (progn
              (print-cell i cell indent)
              (if (cell-functor-p cell)
                (setf indent (wam-functor-arity wam (cell-value cell)))
                (when (not (zerop indent))
                  (decf indent))))))
    (when (< to (wam-heap-pointer wam))
      (format t "  |    ⋮ |  ⋮  |                ⋮ |                                      |~%"))
    (format t "  +------+-----+------------------+--------------------------------------+~%")
    (values)))


(defun dump-stack (wam)
  (format t "STACK~%")
  (format t "  +------+------------------+-------------------------------+~%")
  (format t "  | ADDR |            VALUE |                               |~%")
  (format t "  +------+------------------+-------------------------------+~%")
  (with-accessors ((e wam-environment-pointer)
                   (b wam-backtrack-pointer))
      wam
    (when (not (= +stack-start+ e b))
      (loop
        :with nargs = nil
        :with arg = 0
        :with currently-in = nil
        :for addr :from (1+ +stack-start+) :below (wam-stack-top wam)
        :for cell = (wam-stack-word wam addr)
        :for offset = 0 :then (1+ offset)
        :do
        (when (not (zerop addr))
          (switch (addr :test #'=)
            (e (setf currently-in :frame offset 0 arg 0))
            (b (setf currently-in :choice offset 0 arg 0))))
        (format t "  | ~4,'0X | ~16,'0X | ~30A|~A~A~%"
                addr
                cell
                (case currently-in ; jesus christ this needs to get fixed
                  (:frame
                   (cond
                     ((= addr +stack-start+) "")
                     ((= offset 0) "CE ===========================")
                     ((= offset 1) "CP")
                     ((= offset 2) "CUT")
                     ((= offset 3)
                      (if (zerop cell)
                        (progn
                          (setf currently-in nil)
                          "N: EMPTY")
                        (progn
                          (setf nargs cell)
                          (format nil "N: ~D" cell))))
                     ((< arg nargs)
                      (prog1
                          (format nil " Y~D: ~8,'0X ~A"
                                  arg cell (cell-aesthetic cell))
                        (when (= nargs (incf arg))
                          (setf currently-in nil))))))
                  (:choice ; sweet lord make it stop
                   (cond
                     ((= addr +stack-start+) "")
                     ((= offset 0)
                      (if (zerop cell)
                        (progn
                          (setf currently-in nil)
                          "N: EMPTY =================")
                        (progn
                          (setf nargs cell)
                          (format nil "N: ~D =============" cell))))
                     ((= offset 1) "CE saved env pointer")
                     ((= offset 2) "CP saved cont pointer")
                     ((= offset 3) "CB previous choice")
                     ((= offset 4) "BP next clause")
                     ((= offset 5) "TR saved trail pointer")
                     ((= offset 6) "H  saved heap pointer")
                     ((< arg nargs)
                      (prog1
                          (format nil " A~D: ~8,'0X ~A"
                                  arg cell (cell-aesthetic cell))
                        (when (= nargs (incf arg))
                          (setf currently-in nil))))))
                  (t ""))
                (if (= addr e) " <- E" "")
                (if (= addr b) " <- B" "")))))
  (format t "  +------+------------------+-------------------------------+~%"))


(defun pretty-functor (functor-index functor-list)
  (when functor-list
    (destructuring-bind (symbol . arity)
        (elt functor-list functor-index)
      (format nil "~A/~D" symbol arity))))

(defun pretty-argument (argument)
  (typecase argument
    (fixnum (format nil "~4,'0X" argument))
    (t (format nil "#<*>"))))

(defun pretty-arguments (arguments)
  (format nil "~10<~{ ~A~}~;~>" (mapcar #'pretty-argument arguments)))


(defgeneric instruction-details (opcode arguments functor-list))

(defmethod instruction-details ((opcode t) arguments functor-list)
  (format nil "~A~A"
          (opcode-short-name opcode)
          (pretty-arguments arguments)))


(defmethod instruction-details ((opcode (eql +opcode-get-structure+)) arguments functor-list)
  (format nil "GETS~A ; X~A = ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments) functor-list)))

(defmethod instruction-details ((opcode (eql +opcode-put-structure+)) arguments functor-list)
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
  (format nil "CALL~A ; call ~A"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-jump+)) arguments functor-list)
  (format nil "JUMP~A ; jump ~A"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-dynamic-call+)) arguments functor-list)
  (format nil "DYCL~A ; dynamic call"
          (pretty-arguments arguments)))

(defmethod instruction-details ((opcode (eql +opcode-dynamic-jump+)) arguments functor-list)
  (format nil "DYJP~A ; dynamic jump"
          (pretty-arguments arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-constant+)) arguments functor-list)
  (format nil "GCON~A ; X~A = CONSTANT ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments) functor-list)))

(defmethod instruction-details ((opcode (eql +opcode-put-constant+)) arguments functor-list)
  (format nil "PCON~A ; X~A <- CONSTANT ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments) functor-list)))

(defmethod instruction-details ((opcode (eql +opcode-subterm-constant+)) arguments functor-list)
  (format nil "SCON~A ; SUBTERM CONSTANT ~A"
          (pretty-arguments arguments)
          (pretty-functor (first arguments) functor-list)))

(defmethod instruction-details ((opcode (eql +opcode-get-list+)) arguments functor-list)
  (format nil "GLST~A ; X~A = [vvv | vvv]"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-list+)) arguments functor-list)
  (format nil "PLST~A ; X~A = [vvv | vvv]"
          (pretty-arguments arguments)
          (first arguments)))


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
          (when (not (= +opcode-noop+ (aref instruction 0)))

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
                                                  (wam-functors wam)))))
        (incf addr (length instruction))))))

(defun dump-code
    (wam
     &optional
     (from (max (- (wam-program-counter wam) 8) ; wow
                0)) ; this
     (to (min (+ (wam-program-counter wam) 8) ; is
              (length (wam-code wam))))) ; bad
  (format t "CODE (size: ~D frame(s) / ~:[OPEN~;CLOSED~])~%"
          (length (wam-logic-stack wam))
          (wam-logic-closed-p wam))
  (dump-code-store wam (wam-code wam) from to))


(defun dump-wam-registers (wam)
  (format t "REGISTERS:~%")
  (format t  "~5@A -> ~8X~%" "S" (wam-subterm wam))
  (loop :for register :from 0 :to +register-count+
        :for contents :across (wam-store wam)
        :when (not (cell-null-p contents))
        :do
        (format t "~5@A -> ~8,'0X ~10A ~A~%"
                (format nil "X~D" register)
                contents
                (cell-aesthetic contents)
                (format nil "; ~A" (first (extract-things wam (list register)))))))

(defun dump-wam-functors (wam)
  (format t "        FUNCTORS: ~S~%" (wam-functors wam)))

(defun dump-wam-trail (wam)
  (format t "    TRAIL: ")
  (loop :for addr :across (wam-trail wam) :do
        (format t "~4,'0X ~A //"
                addr
                (cell-aesthetic (wam-store-cell wam addr))))
  (format t "~%"))

(defun dump-labels (wam)
  (format t "LABELS:~%~{  ~A -> ~4,'0X~^~%~}~%"
          (loop :for functor-index
                :being :the :hash-keys :of (wam-code-labels wam)
                :using (hash-value address)
                :nconc (list (pretty-functor functor-index
                                             (wam-functors wam))
                             address))))


(defun dump-wam (wam from to highlight)
  (format t "            FAIL: ~A~%" (wam-fail wam))
  (format t "    BACKTRACKED?: ~A~%" (wam-backtracked wam))
  (format t "            MODE: ~S~%" (wam-mode wam))
  (dump-wam-functors wam)
  (format t "       HEAP SIZE: ~A~%" (- (wam-heap-pointer wam) +heap-start+))
  (format t " PROGRAM COUNTER: ~4,'0X~%" (wam-program-counter wam))
  (format t "CONTINUATION PTR: ~4,'0X~%" (wam-continuation-pointer wam))
  (format t " ENVIRONMENT PTR: ~4,'0X~%" (wam-environment-pointer wam))
  (format t "   BACKTRACK PTR: ~4,'0X~%" (wam-backtrack-pointer wam))
  (format t "         CUT PTR: ~4,'0X~%" (wam-cut-pointer wam))
  (format t "HEAP BCKTRCK PTR: ~4,'0X~%" (wam-heap-backtrack-pointer wam))
  (dump-wam-trail wam)
  (dump-wam-registers wam)
  (format t "~%")
  (dump-heap wam from to highlight)
  (format t "~%")
  (dump-stack wam)
  (format t "~%")
  (dump-labels wam)
  (dump-code wam))

(defun dump-wam-query-code (wam &optional (max +maximum-query-size+))
  (with-slots (code) wam
    (dump-code-store wam code 0 max)))

(defun dump-wam-code (wam)
  (with-slots (code) wam
    (dump-code-store wam code +maximum-query-size+ (length code))))

(defun dump-wam-full (wam)
  (dump-wam wam +heap-start+ (wam-heap-pointer wam) -1))

(defun dump-wam-around (wam addr width)
  (dump-wam wam
            (max +heap-start+ (- addr width))
            (min (wam-heap-pointer wam)
                 (+ addr width 1))
            addr))


