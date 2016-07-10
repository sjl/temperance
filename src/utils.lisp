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


(defmacro yolo (&body body)
  `(locally
     #+sbcl (declare (optimize (sb-c::insert-array-bounds-checks 0)))
     ,@body))


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



