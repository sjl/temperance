(let ((*standard-output* (make-broadcast-stream)))
  (ql:quickload "bones-test"))


(5am:run! :bones)
(5am:run! :bones.paip)
