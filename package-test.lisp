(defpackage :temperance-test
  (:use :cl :1am))

(defpackage :temperance-test.utils
  (:use
    :cl
    :1am
    :temperance
    :temperance.quickutils)
  (:export
    :fail
    :empty
    :result=
    :results=
    :should-fail
    :should-return
    :define-test
    :%=
    :%not
    :%append
    :%member))

(defpackage :temperance-test.wam
  (:use
    :cl
    :1am
    :temperance-test.utils
    :temperance.quickutils
    :temperance)
  (:import-from :temperance
    :with-database
    :make-database
    :with-fresh-database
    :push-logic-frame-with
    :rule
    :fact
    :facts
    :call
    :dump-wam-full
    :?
    :!
    :query
    :query-all))

(defpackage :temperance-test.99
  (:use
    :cl
    :1am
    :temperance-test.utils
    :temperance.quickutils
    :temperance)
  (:import-from :temperance
    :with-fresh-database
    :push-logic-frame-with
    :rule
    :fact
    :facts
    :call
    :dump-wam-full
    :?
    :!
    :query
    :query-all))

(defpackage :temperance-test.taop
  (:use
    :cl
    :1am
    :temperance-test.utils
    :temperance.quickutils
    :temperance)
  (:import-from :temperance
    :with-fresh-database
    :push-logic-frame-with
    :rule
    :fact
    :facts
    :call
    :dump-wam-full
    :?
    :!
    :query
    :query-all))

(defpackage :temperance-test.circle
  (:use
    :cl
    :1am
    :temperance-test.utils
    :temperance.circle))
