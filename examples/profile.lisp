(ql:quickload 'bones)
(load "examples/ggp-wam.lisp")

(require :sb-sprof)

(in-package :bones.wam)

(defun reload ()
  (let ((*standard-output* (make-broadcast-stream))
        (*debug-io* (make-broadcast-stream))
        (*terminal-io* (make-broadcast-stream))
        (*error-output* (make-broadcast-stream)))
    (asdf:load-system 'bones :force t)
    (load "examples/ggp-wam.lisp")))


(defun run-profile ()
  (reload)

  (format t "PROFILING -------------------------------~%")

  ; (sb-sprof:profile-call-counts "COMMON-LISP")
  (sb-sprof:profile-call-counts "BONES.WAM")
  (sb-sprof:profile-call-counts "BONES.QUICKUTILS")

  (sb-sprof:with-profiling (:max-samples 5000
                            :sample-interval 0.0005
                            :loop nil)
    (bones.wam::depth-first-search :exhaust t))

  (sb-sprof:report :type :flat)
  )

; (format t "~%~%====================================~%")
; (format t "(speed 3) (safety 1) (debug 1)~%")
; (declaim (optimize (speed 3) (safety 1) (debug 1)))
; (run-test)

; (format t "~%~%====================================~%")
; (format t "(speed 3) (safety 1) (debug 0)~%")
; (declaim (optimize (speed 3) (safety 3) (debug 3)))
; (run-profile)

; (format t "~%~%====================================~%")
; (format t "(speed 3) (safety 0) (debug 0)~%")
(declaim (optimize (speed 3) (safety 0) (debug 0)))
(run-profile)
