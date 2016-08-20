#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(let ((*standard-output* (make-broadcast-stream))
      ; (*error-output* (make-broadcast-stream))
      )
  (ql:quickload 'bones)
  ;; Recompile to ensure we get the right optimize declarations...
  (asdf:load-system 'bones :force t)
  (ql:quickload 'bones-test))

(time (prog1
          (asdf:test-system 'bones)
        (terpri)))
(terpri)
(quit)
