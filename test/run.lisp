(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "bones-test"))


(defvar *passed* t)

(defun test (spec)
  (let ((result (5am:run spec)))
    (5am:explain! result)
    (when (not (5am:results-status result))
      (setf *passed* nil))))

(test :bones)
(test :bones.paip)
(test :bones.wam)
(test :bones.circle)

(let ((exit-code (if *passed* 0 1)))
  #+sbcl (sb-ext:exit :code exit-code)
  #+ccl (quit exit-code))
