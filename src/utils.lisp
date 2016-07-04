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


(defmacro when-let ((symbol value) &body body)
  `(let ((,symbol ,value))
     (when ,symbol ,@body)))


(defun unique-items (list)
  (loop
    :with once = nil
    :with seen = nil
    :for item :in list
    :do (if (member item seen)
          (when (member item once)
            (setf once (delete item once)))
          (progn (push item seen)
                 (push item once)))
    :finally (return once)))


;;;; loop/recur
(defmacro recursively (bindings &body body)
  "Execute body recursively, like Clojure's `loop`/`recur`.

  `bindings` should contain a list of symbols and (optional) default values.

  In `body`, `recur` will be bound to the function for recurring.

  Example:

      (defun length (some-list)
        (recursively ((list some-list) (n 0))
          (if (null list)
            n
            (recur (cdr list) (1+ n)))))

  "
  (flet ((extract-var (binding)
           (if (atom binding) binding (first binding)))
         (extract-val (binding)
           (if (atom binding) nil (second binding))))
    `(labels ((recur ,(mapcar #'extract-var bindings)
                ,@body))
      (recur ,@(mapcar #'extract-val bindings)))))
