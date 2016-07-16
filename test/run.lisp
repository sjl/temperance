#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(declaim (optimize (debug 3) (safety 3) (speed 0)))


(let ((*standard-output* (make-broadcast-stream))
      ; (*error-output* (make-broadcast-stream))
      )
  (asdf:load-system 'bones :force t)
  (ql:quickload "bones-test"))

(defun done (exit-code)
  #+sbcl (sb-ext:exit :code exit-code)
  #+ccl (quit exit-code)
  #+ecl (quit exit-code))


(time (progn (1am:run) (terpri)))
(terpri)
(done 0)
