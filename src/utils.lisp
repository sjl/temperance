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

(defun invert-hash-table (hash-table)
  "Jesus christ don't actually use this for anything but debugging.

  Inverts the keys/values of a `hash-table`.

  "
  (alist-to-hash-table
    (loop :for k :being :the :hash-keys :of hash-table
          :using (hash-value v)
          :collect (list v k))))

(defmacro repeat (n &body body)
  "Repeat `body` `n` times."
  `(dotimes (,(gensym) ,n)
     ,@body))

(defun hex (d)
  (format nil "~X" d))

(defmacro when-let ((symbol value) &body body)
  "Bind `value` to `symbol` and execute `body` if the value was not `nil`."
  `(let ((,symbol ,value))
     (when ,symbol ,@body)))

(defun unique-items (list)
  "Return a list of the items that appear exactly once in `list`."
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

(defmacro dis (arglist &body body)
  "Disassemble the code generated for a `lambda*` with `arglist` and `body`.

  It will also spew compiler notes so you can see why the garbage box isn't
  doing what you think it should be doing.

  "
  `(->> '(lambda* ,arglist
          (declare (optimize speed))
          ,@body)
    macroexpand-1
    (compile nil)
    disassemble))

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

(defmacro gethash-or-init (key hash-table default-form)
  "Get the a key's value in a hash table, initializing if necessary.

  If `key` is in `hash-table`: return its value without evaluating
  `default-form` at all.

  If `key` is NOT in `hash-table`: evaluate `default-form` and insert it before
  returning it.

  "
  ;; TODO: think up a less shitty name for this
  (once-only (key hash-table)
    (with-gensyms (value found)
      `(multiple-value-bind (,value ,found)
        (gethash ,key ,hash-table)
        (if ,found
          ,value
          (setf (gethash ,key ,hash-table) ,default-form))))))

(defmacro array-push (value array pointer &environment env)
  "Push `value` onto `array` at `pointer`, incrementing `pointer` afterword.

  Returns the index the value was pushed to.

  "
  (multiple-value-bind (temp-vars temp-vals stores store access)
      (get-setf-expansion pointer env)
    (with-gensyms (address)
      `(let* (,@(mapcar #'list temp-vars temp-vals)
              (,address ,access)
              (,(car stores) (1+ ,address)))
        (setf (aref ,array ,address) ,value)
        ,store
        ,address))))

(defmacro yolo (&body body)
  `(locally
     #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)
                               (speed 3)
                               (debug 0)
                               (safety 0)))
     ,@body))

(defun megabytes (n)
  "Return the number of 64-bit words in `n` megabytes."
  (* 1024 1024 1/8 n))


;;;; Queues
;;; From PAIP (thanks, Norvig).

(deftype queue () '(cons list list))
(declaim (inline queue-contents make-queue
                 enqueue dequeue
                 queue-empty-p queue-append))


(defun* queue-contents ((q queue))
  (:returns list)
  (cdr q))

(defun* make-queue ()
  (:returns queue)
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun* enqueue ((item t) (q queue))
  (:returns queue)
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun* dequeue ((q queue))
  (:returns t)
  (prog1
      (pop (cdr q))
    (if (null (cdr q))
      (setf (car q) q))))

(defun* queue-empty-p ((q queue))
  (:returns boolean)
  (null (queue-contents q)))

(defun* queue-append ((q queue) (l list))
  (:returns queue)
  (when l
    (setf (car q)
          (last (setf (rest (car q))
                      l))))
  q)


;;;; Lookup Tables
(defmacro define-lookup
    (name (key key-type value-type default) documentation &rest entries)
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
  function.  `key-type` should be its type and should be a subtype of
  (integer 0 some-small-number) if you want this to be efficient.

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
        (defun* ,name ((,key ,key-type))
          (:returns ,value-type)
          ,documentation
          (aref ,table ,key))))))
