(in-package #:bones.wam)

(defun heap-debug (wam address indent-p)
  (format
    nil "~A~A"
    (if indent-p
      "  "
      "")
    (cell-typecase (wam address)
      ((:reference r) (if (= address r)
                        "unbound variable "
                        (format nil "var pointer to ~8,'0X " r)))
      ((:structure s) (format nil "struct pointer to ~8,'0X " s))
      ((:functor f) (destructuring-bind (functor . arity) f
                      (format nil "~A/~D " functor arity)))
      ((:constant c) (format nil "~A/0 " (car c)))
      (t ""))))


(defun dump-cell-value (value)
  ;; todo flesh this out
  (typecase value
    (fixnum (format nil "~16,'0X" value))
    (t (format nil "~16<#<lisp object>~;~>"))))


(defun dump-heap (wam from to)
  ;; This code is awful, sorry.
  (format t "HEAP~%")
  (format t "  +----------+-----+------------------+--------------------------------------+~%")
  (format t "  | ADDR     | TYP |            VALUE | DEBUG                                |~%")
  (format t "  +----------+-----+------------------+--------------------------------------+~%")
  (when (> from (1+ +heap-start+))
    (format t "  | ⋮        |  ⋮  |                ⋮ |                                      |~%"))
  (flet ((print-cell (address indent)
           (format t "  | ~8,'0X | ~A | ~16,'0X | ~36A |~%"
                   address
                   (cell-type-short-name (wam-store-type wam address))
                   (dump-cell-value (wam-store-value wam address))
                   (heap-debug wam address (plusp indent)))))
    (loop :with indent = 0
          :for address :from from :below to
          :do (progn
                (print-cell address indent)
                (cell-typecase (wam address)
                  ((:functor f) (setf indent (cdr f)))
                  (t (when (not (zerop indent))
                       (decf indent)))))))
  (when (< to (wam-heap-pointer wam))
    (format t "  | ⋮        |  ⋮  |                ⋮ |                                      |~%"))
  (format t "  +----------+-----+------------------+--------------------------------------+~%")
  (values))


(defun dump-stack-frame (wam start-address)
  (loop :with remaining = nil
        :with arg-number = nil
        :for address :from start-address
        :for offset :from 0
        :for type = (wam-store-type wam address)
        :for value = (wam-store-value wam address)
        :while (or (null remaining) (plusp remaining))
        :do (format
              t "  | ~8,'0X | ~A | ~30A|~A~A~A~%"
              address
              (dump-cell-value value)
              (cond
                ((= address +stack-start+) "")
                ((= offset 0) "CE ===========================")
                ((= offset 1) "CP")
                ((= offset 2) "CUT")
                ((= offset 3) (progn
                                (setf remaining value
                                      arg-number 0)
                                (format nil "N: ~D" value)))
                (t (prog1
                       (format nil " Y~D: ~A ~A"
                               arg-number
                               (cell-type-short-name type)
                               (dump-cell-value value))
                       (decf remaining)
                       (incf arg-number))))
              (if (= address (wam-environment-pointer wam)) " <- E" "")
              (if (= address (wam-backtrack-pointer wam)) " <- B" "")
              (if (= address (wam-cut-pointer wam)) " <- CUT" ""))
        :finally (return address)))

(defun dump-stack-choice (wam start-address)
  (loop :with remaining = nil
        :with arg-number = nil
        :for address :from start-address
        :for offset :from 0
        :for type = (wam-store-type wam address)
        :for value = (wam-store-value wam address)
        :while (or (null remaining) (plusp remaining))
        :do (format
              t "  | ~8,'0X | ~A | ~30A|~A~A~A~%"
              address
              (dump-cell-value value)
              (cond
                ((= address +stack-start+) "")
                ((= offset 0) (progn
                                (setf remaining value
                                      arg-number 0)
                                (format nil "N: ~D =============" value)))
                ((= offset 1) "CE saved env pointer")
                ((= offset 2) "CP saved cont pointer")
                ((= offset 3) "CB previous choice")
                ((= offset 4) "BP next clause")
                ((= offset 5) "TR saved trail pointer")
                ((= offset 6) "H  saved heap pointer")
                (t (prog1
                       (format nil " A~D: ~A ~A"
                               arg-number
                               (cell-type-short-name type)
                               (dump-cell-value value))
                     (decf remaining)
                     (incf arg-number))))
              (if (= address (wam-environment-pointer wam)) " <- E" "")
              (if (= address (wam-backtrack-pointer wam)) " <- B" "")
              (if (= address (wam-cut-pointer wam)) " <- CUT" ""))
        :finally (return address)))

(defun dump-stack (wam)
  (format t "STACK~%")
  (format t "  +----------+------------------+-------------------------------+~%")
  (format t "  | ADDR     |            VALUE |                               |~%")
  (format t "  +----------+------------------+-------------------------------+~%")
  (with-accessors ((e wam-environment-pointer) (b wam-backtrack-pointer)) wam
    (when (not (= +stack-start+ e b))
      (loop :with address = (1+ +stack-start+)
            :while (< address (wam-stack-top wam))
            :do (cond
                  ((= address e) (setf address (dump-stack-frame wam address)))
                  ((= address b) (setf address (dump-stack-choice wam address)))
                  (t
                   (format t "  | ~8,'0X | | |~%" address)
                   (incf address))))))
  (format t "  +----------+------------------+-------------------------------+~%"))


(defun pretty-functor (functor)
  (destructuring-bind (symbol . arity) functor
    (format nil "~A/~D" symbol arity)))

(defun pretty-argument (argument)
  (typecase argument
    (fixnum (format nil "~4,'0X" argument))
    (t (format nil "#<*>"))))

(defun pretty-arguments (arguments)
  (format nil "~10<~{ ~A~}~;~>" (mapcar #'pretty-argument arguments)))


(defgeneric instruction-details (opcode arguments))

(defmethod instruction-details ((opcode t) arguments)
  (format nil "~A~A"
          (opcode-short-name opcode)
          (pretty-arguments arguments)))


(defmethod instruction-details ((opcode (eql +opcode-get-structure+)) arguments)
  (format nil "GETS~A ; X~A = ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-put-structure+)) arguments)
  (format nil "PUTS~A ; X~A <- new ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-get-variable-local+)) arguments)
  (format nil "GVAR~A ; X~A <- A~A"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-variable-stack+)) arguments)
  (format nil "GVAR~A ; Y~A <- A~A"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-value-local+)) arguments)
  (format nil "GVLU~A ; X~A = A~A"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-value-stack+)) arguments)
  (format nil "GVLU~A ; Y~A = A~A"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-variable-local+)) arguments)
  (format nil "PVAR~A ; X~A <- A~A <- new unbound REF"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-variable-stack+)) arguments)
  (format nil "PVAR~A ; Y~A <- A~A <- new unbound REF"
          (pretty-arguments arguments)
          (first arguments)
          (second arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-value-local+)) arguments)
  (format nil "PVLU~A ; A~A <- X~A"
          (pretty-arguments arguments)
          (second arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-value-stack+)) arguments)
  (format nil "PVLU~A ; A~A <- Y~A"
          (pretty-arguments arguments)
          (second arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-call+)) arguments)
  (format nil "CALL~A ; call ~A"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-jump+)) arguments)
  (format nil "JUMP~A ; jump ~A"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-dynamic-call+)) arguments)
  (format nil "DYCL~A ; dynamic call"
          (pretty-arguments arguments)))

(defmethod instruction-details ((opcode (eql +opcode-dynamic-jump+)) arguments)
  (format nil "DYJP~A ; dynamic jump"
          (pretty-arguments arguments)))

(defmethod instruction-details ((opcode (eql +opcode-get-constant+)) arguments)
  (format nil "GCON~A ; X~A = CONSTANT ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-put-constant+)) arguments)
  (format nil "PCON~A ; X~A <- CONSTANT ~A"
          (pretty-arguments arguments)
          (second arguments)
          (pretty-functor (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-subterm-constant+)) arguments)
  (format nil "SCON~A ; SUBTERM CONSTANT ~A"
          (pretty-arguments arguments)
          (pretty-functor (first arguments))))

(defmethod instruction-details ((opcode (eql +opcode-get-list+)) arguments)
  (format nil "GLST~A ; X~A = [vvv | vvv]"
          (pretty-arguments arguments)
          (first arguments)))

(defmethod instruction-details ((opcode (eql +opcode-put-list+)) arguments)
  (format nil "PLST~A ; X~A = [vvv | vvv]"
          (pretty-arguments arguments)
          (first arguments)))


(defun functor-table (wam)
  (loop
    :with result = (make-hash-table)
    :for arity :from 0
    :for table :across (wam-code-labels wam)
    :when table
    :do (maphash (lambda (functor loc)
                   (setf (gethash loc result)
                         (cons functor arity)))
                 table)
    :finally (return result)))

(defun dump-code-store (wam code-store
                        &optional
                        (from 0)
                        (to (length code-store)))
  ;; This is a little trickier than might be expected.  We have to walk from
  ;; address 0 no matter what `from` we get, because instruction sizes vary and
  ;; aren't aligned.  So if we just start at `from` we might start in the middle
  ;; of an instruction and everything would be fucked.
  (let ((addr 0)
        (lbls (functor-table wam))) ; oh god
    (while (< addr to)
      (let ((instruction (retrieve-instruction code-store addr)))
        (when (>= addr from)
          (when (not (= +opcode-noop+ (aref instruction 0)))

            (let ((lbl (gethash addr lbls))) ; forgive me
              (when lbl
                (format t ";;;; BEGIN ~A~%"
                        (pretty-functor lbl))))
            (format t ";~A~4,'0X: "
                    (if (= (wam-program-counter wam) addr)
                      ">>"
                      "  ")
                    addr)
            (format t "~A~%" (instruction-details (aref instruction 0)
                                                  (rest (coerce instruction 'list))))))
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
        :for type = (wam-store-type wam register)
        :for value = (wam-store-value wam register)
        :when (not (cell-type-p (wam register) :null))
        :do (format t "~5@A -> ~A ~A ~A~%"
                    (format nil "X~D" register)
                    (cell-type-short-name type)
                    (dump-cell-value value)
                    (format nil "; ~A" (first (extract-things wam (list register)))))))


(defun dump-wam-trail (wam)
  (format t "    TRAIL: ")
  (loop :for address :across (wam-trail wam) :do
        (format t "~8,'0X //" address))
  (format t "~%"))


(defun dump-wam (wam from to)
  (format t "            FAIL: ~A~%" (wam-fail wam))
  (format t "    BACKTRACKED?: ~A~%" (wam-backtracked wam))
  (format t "            MODE: ~S~%" (wam-mode wam))
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
  (dump-heap wam from to)
  (format t "~%")
  (dump-stack wam)
  (format t "~%")
  (dump-code wam))

(defun dump-wam-query-code (wam &optional (max +maximum-query-size+))
  (with-slots (code) wam
    (dump-code-store wam code 0 max)))

(defun dump-wam-code (wam)
  (with-slots (code) wam
    (dump-code-store wam code +maximum-query-size+ (length code))))

(defun dump-wam-full (wam)
  (dump-wam wam (1+ +heap-start+) (wam-heap-pointer wam)))

