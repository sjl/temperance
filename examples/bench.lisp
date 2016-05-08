(ql:quickload 'bones)

(load "examples/ggp-paip.lisp")
(load "examples/ggp.lisp")

(in-package :bones)

(defun reload ()
  (let ((*standard-output* (make-broadcast-stream))
        (*debug-io* (make-broadcast-stream))
        (*terminal-io* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (asdf:load-system 'bones :force t)))

(defun run-test ()
  (reload)

  (format t "PAIP ------------------------------~%")
  (time (bones.paip::dfs-exhaust))

  (format t "WAM -------------------------------~%")
  (time (bones.wam::dfs-exhaust)))

(format t "~%~%====================================~%")
(format t "(speed 3) (safety 1) (debug 1)~%")
(declaim (optimize (speed 3) (safety 1) (debug 1)))
(run-test)

(format t "~%~%====================================~%")
(format t "(speed 3) (safety 1) (debug 0)~%")
(declaim (optimize (speed 3) (safety 1) (debug 0)))
(run-test)

(format t "~%~%====================================~%")
(format t "(speed 3) (safety 0) (debug 0)~%")
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(run-test)
