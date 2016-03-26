(in-package #:bones.wam)

;;;; Topological Sort
;;; Adapted from the AMOP book to add some flexibility (and remove the
;;; tie-breaker functionality, which we don't need).
(defun topological-sort
    (elements constraints &key (key #'identity) (key-test #'eql) (test #'equal))
  "Return a topologically sorted list of `elements` given the `constraints`.

  `elements` should be a sequence of elements to be sorted.

  `constraints` should be a list of `(key . key)` conses where `(foo . bar)`
  means element `foo` must precede `bar` in the result.

  `key` will be used to turn items in `elements` into the keys in `constraints`.

  `key-test` is the equality predicate for keys.

  `test` is the equality predicate for (non-keyified) elements.

  "
  (labels
      ((find-minimal-elements (elements constraints)
         ;; An element is minimal if there are no other elements that are
         ;; required to precede it.
         (remove-if #'(lambda (element)
                       (member (funcall key element)
                               constraints
                               :key #'cdr
                               :test key-test))
                    elements))
       (in-constraint (val constraint)
         ;; Return whether val is either part of a constraint.
         (or (funcall key-test val (car constraint))
             (funcall key-test val (cdr constraint))))
       (recur (remaining-constraints remaining-elements result)
         (let ((minimal-elements (find-minimal-elements remaining-elements
                                                        remaining-constraints)))
           (if (null minimal-elements)
             (if (null remaining-elements)
               result
               (error "Inconsistent constraints."))
             (let ((choice (car minimal-elements)))
               (recur (remove (funcall key choice)
                              remaining-constraints
                              :test #'in-constraint)
                      (remove choice remaining-elements :test test)
                      (cons choice result)))))))
    (reverse (recur constraints elements (list)))))
