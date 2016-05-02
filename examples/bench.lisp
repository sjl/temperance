(declaim (optimize (speed 3) (safety 1) (debug 0)))

(ql:quickload 'bones)

(load "examples/ggp-paip.lisp")
(load "examples/ggp.lisp")

(in-package :bones.paip)
(format t "PAIP ------------------------------~%")
(time (dfs))

(in-package :bones.wam)
(format t "WAM -------------------------------~%")
(time (dfs))
