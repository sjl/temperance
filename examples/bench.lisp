(ql:quickload 'bones)
(ql:quickload 'paiprolog)

(load "examples/ggp-paip-compiled.lisp")
(load "examples/ggp-paip-interpreted.lisp")
(load "examples/ggp-wam.lisp")

(in-package :bones)

(defun reload ()
  (let ((*standard-output* (make-broadcast-stream))
        (*debug-io* (make-broadcast-stream))
        (*terminal-io* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (asdf:load-system 'bones :force t)
    (asdf:load-system 'paiprolog :force t)
    (load "examples/ggp-paip-compiled.lisp")
    (load "examples/ggp-paip-interpreted.lisp")
    (load "examples/ggp-wam.lisp")))

(defun run-test% ()
  ; (format t "PAIP (Compiled) --------------------~%")
  ; (time (paiprolog-test::dfs-exhaust))

  (format t "PAIP (Interpreted) -----------------~%")
  (time (bones.paip::depth-first-search :exhaust t))

  (format t "WAM --------------------------------~%")
  (time (bones.wam::depth-first-search :exhaust t)))

(defmacro run-test (&rest settings)
  `(progn
    (declaim (optimize ,@settings))
    (format t "~%~%========================================================~%")
    (format t "~S~%" ',settings)
    (format t "--------------------------------------------------------~%")
    (reload)
    (run-test%)))

; (run-test (speed 3) (safety 1) (debug 1))
(run-test (speed 3) (safety 0) (debug 0))
