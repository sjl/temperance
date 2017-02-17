#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(declaim (optimize (debug 3) (safety 3) (speed 0)))

(let ((*standard-output* (make-broadcast-stream))
      ; (*error-output* (make-broadcast-stream))
      )
  (ql:quickload 'temperance)
  ;; Recompile to ensure we get the right optimize declarations...
  (asdf:load-system 'temperance :force t)
  (ql:quickload 'temperance.test))

(time (prog1
          (asdf:test-system 'temperance)
        (terpri)))
(terpri)
(quit)
