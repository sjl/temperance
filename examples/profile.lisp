(ql:quickload 'bones)

(require :sb-sprof)

(load "examples/ggp-wam.lisp")

(in-package :bones)

(defun reload ()
  (let ((*standard-output* (make-broadcast-stream))
        (*debug-io* (make-broadcast-stream))
        (*terminal-io* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (asdf:load-system 'bones :force t)))


(defun run-profile ()
  (reload)

  (format t "PROFILING -------------------------------~%")

  (sb-sprof:profile-call-counts "BONES.WAM")

  (sb-sprof:with-profiling (:max-samples 5000
                            :sample-interval 0.001
                            :loop nil)
    (bones.wam::dfs-exhaust))

  (sb-sprof:report :type :flat)
  )

; (format t "~%~%====================================~%")
; (format t "(speed 3) (safety 1) (debug 1)~%")
; (declaim (optimize (speed 3) (safety 1) (debug 1)))
; (run-test)

; (format t "~%~%====================================~%")
; (format t "(speed 3) (safety 1) (debug 0)~%")
; (declaim (optimize (speed 3) (safety 1) (debug 0)))
; (run-test)

(format t "~%~%====================================~%")
(format t "(speed 3) (safety 0) (debug 0)~%")
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(run-profile)
