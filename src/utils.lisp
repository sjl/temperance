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
