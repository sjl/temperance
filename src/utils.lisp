(in-package #:temperance.utils)

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

(defun invert-hash-table (hash-table)
  "Jesus christ don't actually use this for anything but debugging.

  Inverts the keys/values of a `hash-table`.

  "
  (alist-to-hash-table
    (loop :for k :being :the :hash-keys :of hash-table
          :using (hash-value v)
          :collect (list v k))))


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

(defmacro aref-or-init (array index default-form)
  "Get `index` in `array`, initializing if necessary.

  If `index` is non-nil in `array`: return its value without evaluating
  `default-form` at all.

  If `index` is nil in `array`: evaluate `default-form` and set it before
  returning it.

  "
  ;; TODO: think up a less shitty name for this
  (once-only (index array)
    `(or (aref ,array ,index)
      (setf (aref ,array ,index) ,default-form))))


(defun megabytes (n)
  "Return the number of 64-bit words in `n` megabytes."
  (* 1024 1024 1/8 n))


;;;; Queues
;;; Based on the PAIP queues (thanks, Norvig), but beefed up a little bit to add
;;; tracking of the queue size.

(declaim (inline make-queue enqueue dequeue queue-empty-p))

(defstruct (queue (:constructor make-queue%))
  (contents nil :type list)
  (last nil :type list)
  (size 0 :type fixnum))


(defun make-queue ()
  (make-queue%))

(defun queue-empty-p (q)
  (zerop (queue-size q)))

(defun enqueue (item q)
  (let ((cell (cons item nil)))
    (setf (queue-last q)
          (if (queue-empty-p q)
            (setf (queue-contents q) cell)
            (setf (cdr (queue-last q)) cell))))
  (incf (queue-size q)))

(defun dequeue (q)
  (when (zerop (decf (queue-size q)))
    (setf (queue-last q) nil))
  (pop (queue-contents q)))

(defun queue-append (q l)
  (loop :for item :in l
        :for size = (enqueue item q)
        :finally (return size)))


;;;; Lookup Tables
(defmacro define-lookup
    (name (key value-type default) documentation &rest entries)
  "Define a lookup function.

  This macro defines a function that looks up a result in a constant array.
  It's useful for things where you're looking up keys that are small integers,
  like opcodes.

  The function should be compiled to a few ASM instructions to read from a bit
  of memory in O(1) time, instead of a huge list of CMP instructions that's
  O(n) on the number of possibilities.

  `name` should be a symbol that will become the name of the function.  It will
  be munged to make a name for the constant table too, but you shouldn't mess
  with that.

  `key` should be a symbol that will be used as the argument for the lookup
  function.

  `value-type` should be the type of your results.

  `default` should be a value that will be returned from your function if a key
  that does not exist is requested.  Note that this same `eq` value will always
  be returned.

  `entries` should be the list of `(key value)` entries for the table.

  Note that `key`, `default`, and all the keys of `entries` must be
  macroexpansion-time constants!

  "
  (let ((max (reduce #'max entries :key #'car))
        (entries (apply #'append entries)))
    (let ((table (intern (format nil "+~A-TABLE+" name))))
      `(progn
        (define-constant ,table
          (make-array (1+ ,max)
            :element-type ',value-type
            :initial-contents
            (list ,@(loop :for i :from 0 :to max
                          :collect (getf entries i default))))
          :test (lambda (x y) (declare (ignore x y)) t)) ; what could go wrong
        (declaim (inline ,name))
        (defun ,name (,key)
          ,documentation
          (the ,value-type (aref ,table ,key)))))))


;;;; ecase/tree
;;; See http://www.foldr.org/~michaelw/log/programming/lisp/icfp-contest-2006-vm

(defmacro ecase/tree (keyform &body cases)
  (labels ((%case/tree (keyform cases)
             (if (<= (length cases) 4)
                 `(ecase ,keyform ,@cases)
                 (loop for rest-cases on cases
                       repeat (truncate (length cases) 2)
                       collect (first rest-cases) into first-half
                       finally (return `(if (< ,keyform ,(caar rest-cases))
                                            ,(%case/tree keyform first-half)
                                            ,(%case/tree keyform rest-cases)))))))
    (let (($keyform (gensym "CASE/TREE-")))
      `(let ((,$keyform ,keyform))
         ,(%case/tree $keyform (sort (copy-list cases) #'< :key #'first))))))

