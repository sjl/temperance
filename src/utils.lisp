(in-package #:bones.utils)

(defmacro push-if-new (thing place
                             &environment env
                             &key key (test '#'eql))
  "Push `thing` into the list at `place` if it's not already there.

  Returns whether `thing` was actually pushed.  This function is basically
  `pushnew` except for the return value.

  "
  (multiple-value-bind (temps exprs stores store-expr access-expr)
      (get-setf-expansion place env)
    (declare (ignore stores store-expr))
    (with-gensyms (current result)
      `(let* (,@(zip temps exprs)
              (,current ,access-expr)
              (,result (pushnew ,thing ,place :key ,key :test ,test)))
        (not (eql ,current ,result))))))

(defun invert-hash-table (ht)
  "Jesus christ don't actually use this for anything but debugging.

  Inverts the keys/values of a hash table.

  "
  (alist-to-hash-table
    (loop :for k :being :the :hash-keys :of ht
          :using (hash-value v)
          :collect (list v k))))

(defmacro repeat (n &body body)
  "Repeat `body` `n` times."
  `(dotimes (,(gensym) ,n)
     ,@body))

(defun hex (d)
  (format nil "~X" d))


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
      ((minimal-p (element constraints)
         ;; An element is minimal if there are no other elements that must
         ;; precede it.
         (not (member (funcall key element) constraints
                      :key #'cdr
                      :test key-test)))
       (in-constraint (val constraint)
         ;; Return whether val is either part of a constraint.
         (or (funcall key-test val (car constraint))
             (funcall key-test val (cdr constraint))))
       (recur (remaining-constraints remaining-elements result)
         (let ((minimal-element
                 (find-if (lambda (el)
                            (minimal-p el remaining-constraints))
                          remaining-elements)))
           (if (null minimal-element)
             (if (null remaining-elements)
               result
               (error "Inconsistent constraints."))
             (recur (remove (funcall key minimal-element)
                            remaining-constraints
                            :test #'in-constraint)
                    (remove minimal-element remaining-elements :test test)
                    (cons minimal-element result))))))
    (reverse (recur constraints elements (list)))))
