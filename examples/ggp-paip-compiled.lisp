(defpackage #:paiprolog-test
  (:use #:cl #:paiprolog))

(in-package #:paiprolog-test)


(defvar *state* nil)
(defvar *actions* nil)

(defun paiprolog::true/1 (?thing cont)
  (loop :with tr = (fill-pointer paiprolog::*trail*)
        :for item :in *state*
        :when (paiprolog::unify! ?thing item)
        :do
        (funcall cont)
        (paiprolog::undo-bindings! tr)))

(defun paiprolog::does/1 (?action cont)
  (loop :with tr = (fill-pointer paiprolog::*trail*)
        :for action :in *actions*
        :when (paiprolog::unify! ?action action)
        :do
        (funcall cont)
        (paiprolog::undo-bindings! tr)))

(<-- (member ?x (?x . ?)))
(<- (member ?x (?y . ?rest))
    (member ?x ?rest))

(<-- (role robot))

(<-- (init (off p)))
(<- (init (off q)))
(<- (init (off r)))
(<- (init (off s)))
(<- (init (step num1)))

(<-- (next (on p))
     (does (robot a))
     (true (off p)))
(<- (next (on q))
    (does (robot a))
    (true (on q)))
(<- (next (on r))
    (does (robot a))
    (true (on r)))
(<- (next (off p))
    (does (robot a))
    (true (on p)))
(<- (next (off q))
    (does (robot a))
    (true (off q)))
(<- (next (off r))
    (does (robot a))
    (true (off r)))

(<- (next (on p))
    (does (robot b))
    (true (on q)))
(<- (next (on q))
    (does (robot b))
    (true (on p)))
(<- (next (on r))
    (does (robot b))
    (true (on r)))
(<- (next (off p))
    (does (robot b))
    (true (off q)))
(<- (next (off q))
    (does (robot b))
    (true (off p)))
(<- (next (off r))
    (does (robot b))
    (true (off r)))

(<- (next (on p))
    (does (robot c))
    (true (on p)))
(<- (next (on q))
    (does (robot c))
    (true (on r)))
(<- (next (on r))
    (does (robot c))
    (true (on q)))
(<- (next (off p))
    (does (robot c))
    (true (off p)))
(<- (next (off q))
    (does (robot c))
    (true (off r)))
(<- (next (off r))
    (does (robot c))
    (true (off q)))

(<- (next (off s))
    (does (robot a))
    (true (off s)))
(<- (next (off s))
    (does (robot b))
    (true (off s)))
(<- (next (off s))
    (does (robot c))
    (true (off s)))
(<- (next (on s))
    (does (robot a))
    (true (on s)))
(<- (next (on s))
    (does (robot b))
    (true (on s)))
(<- (next (on s))
    (does (robot c))
    (true (on s)))
(<- (next (off s))
    (does (robot d))
    (true (on s)))
(<- (next (on s))
    (does (robot d))
    (true (off s)))

(<- (next (on p))
    (does (robot d))
    (true (on p)))
(<- (next (off p))
    (does (robot d))
    (true (off p)))

(<- (next (on q))
    (does (robot d))
    (true (on q)))
(<- (next (off q))
    (does (robot d))
    (true (off q)))

(<- (next (on r))
    (does (robot d))
    (true (on r)))
(<- (next (off r))
    (does (robot d))
    (true (off r)))

(<- (next (step ?y))
    (true (step ?x))
    (succ ?x ?y))

(<-- (succ num1 num2))
(<- (succ num2 num3))
(<- (succ num3 num4))
(<- (succ num4 num5))
(<- (succ num5 num6))
(<- (succ num6 num7))
(<- (succ num7 num8))

(<-- (legal robot a))
(<- (legal robot b))
(<- (legal robot c))
(<- (legal robot d))

(<-- (goal robot num100)
     (true (on p))
     (true (on q))
     (true (on r))
     (true (on s)))
(<- (goal robot num0)
    (true (off p)))
(<- (goal robot num0)
    (true (off q)))
(<- (goal robot num0)
    (true (off r)))
(<- (goal robot num0)
    (true (off s)))

(<-- (terminal)
     (true (step num8)))
(<- (terminal)
    (true (on p))
    (true (on q))
    (true (on r))
    (true (on s)))

(<-- (lol 1))


(defvar *count* 0)

(defun initial-state ()
  (prolog-collect (?what) (init ?what)))


(defun terminalp ()
  (not (null (prolog-first (?lol)
                           (terminal)
                           (lol ?lol)))))

(defun legal-moves (state)
  (declare (ignore state))
  (prolog-collect (?role ?move) (legal ?role ?move)))

(defun roles ()
  (prolog-collect (?role) (role ?role)))

(defun goal-value ()
  (prolog-first (?goal) (goal robot ?goal)))

(defun next-state (move)
  (setf *actions* (list move))
  (prolog-collect (?what) (next ?what)))


(defstruct search-path state (path nil) (previous nil))

(defun tree-search (states goal-p children combine)
  (labels
      ((recur (states)
         (if (null states)
           nil
           (destructuring-bind (state . remaining) states
             (incf *count*)
             ; (format t "Searching: ~S (~D remaining)~%" state (length remaining))
             (if (funcall goal-p state)
               state
               (recur (funcall combine
                               (funcall children state)
                               remaining)))))))
    (let ((result (recur states)))
      (when result
        (reverse (search-path-path result))))))


(defun buttons-goal-p (search-path)
  (setf *state* (search-path-state search-path))
  (and (terminalp)
       (eql (goal-value) 'num100)))

(defun buttons-children (search-path)
  (let ((state (search-path-state search-path))
        (path (search-path-path search-path)))
    (setf *state* state)
    (when (not (terminalp))
      (loop :for move :in (legal-moves state)
            :collect (make-search-path :state (next-state move)
                                       :path (cons move path)
                                       :previous search-path)))))

(defun never (&rest args)
  (declare (ignore args))
  nil)

(defun dfs ()
  (tree-search (list (make-search-path :state (initial-state)))
               #'buttons-goal-p
               #'buttons-children
               #'append))

(defun dfs-exhaust ()
  (let ((*count* 0))
    (prog1
        (tree-search (list (make-search-path :state (initial-state)))
                     #'never
                     #'buttons-children
                     #'append)
      (format t "Searched ~D nodes.~%" *count*))))

(defun bfs ()
  (tree-search (list (make-search-path :state (initial-state)))
               #'buttons-goal-p
               #'buttons-children
               (lambda (x y)
                 (append y x))))


(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

#+no
(progn
  (require :sb-sprof)
  (sb-sprof:with-profiling (:max-samples 10000
                            :sample-interval 0.01
                            :loop nil)
    (dfs-exhaust))

  (sb-sprof:report :type :flat :max 100))
