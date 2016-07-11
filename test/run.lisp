(declaim (optimize (debug 3) (safety 3) (speed 0)))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (asdf:load-system 'bones :force t)
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
