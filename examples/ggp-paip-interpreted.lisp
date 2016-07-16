(in-package #:bones.paip)

;;;; Queues
(deftype queue () '(cons list list))
(declaim (inline queue-contents make-queue
                 enqueue dequeue
                 queue-empty-p queue-append))


(defun queue-contents (q)
  (cdr q))

(defun make-queue ()
  (let ((q (cons nil nil)))
    (setf (car q) q)))

(defun enqueue (item q)
  (setf (car q)
        (setf (rest (car q))
              (cons item nil)))
  q)

(defun dequeue (q)
  (prog1
      (pop (cdr q))
    (if (null (cdr q))
      (setf (car q) q))))

(defun queue-empty-p (q)
  (null (queue-contents q)))

(defun queue-append (q l)
  (when l
    (setf (car q)
          (last (setf (rest (car q))
                      l))))
  q)


;;;; Rules
(clear-db)

(rule (member ?thing (cons ?thing ?rest)))

(rule (member ?thing (cons ?other ?rest))
  (member ?thing ?rest))

(rule (true ?state ?thing)
  (member ?thing ?state))

(rule (does ?performed ?role ?move)
  (member (does ?role ?move) ?performed))

(rule (role robot))

(rule (init (off p)))
(rule (init (off q)))
(rule (init (off r)))
(rule (init (off s)))
(rule (init (step num1)))

(rule (next ?state ?performed (on p))
  (does ?performed robot a)
  (true ?state (off p)))
(rule (next ?state ?performed (on q))
  (does ?performed robot a)
  (true ?state (on q)))
(rule (next ?state ?performed (on r))
  (does ?performed robot a)
  (true ?state (on r)))
(rule (next ?state ?performed (off p))
  (does ?performed robot a)
  (true ?state (on p)))
(rule (next ?state ?performed (off q))
  (does ?performed robot a)
  (true ?state (off q)))
(rule (next ?state ?performed (off r))
  (does ?performed robot a)
  (true ?state (off r)))

(rule (next ?state ?performed (on p))
  (does ?performed robot b)
  (true ?state (on q)))
(rule (next ?state ?performed (on q))
  (does ?performed robot b)
  (true ?state (on p)))
(rule (next ?state ?performed (on r))
  (does ?performed robot b)
  (true ?state (on r)))
(rule (next ?state ?performed (off p))
  (does ?performed robot b)
  (true ?state (off q)))
(rule (next ?state ?performed (off q))
  (does ?performed robot b)
  (true ?state (off p)))
(rule (next ?state ?performed (off r))
  (does ?performed robot b)
  (true ?state (off r)))

(rule (next ?state ?performed (on p))
  (does ?performed robot c)
  (true ?state (on p)))
(rule (next ?state ?performed (on q))
  (does ?performed robot c)
  (true ?state (on r)))
(rule (next ?state ?performed (on r))
  (does ?performed robot c)
  (true ?state (on q)))
(rule (next ?state ?performed (off p))
  (does ?performed robot c)
  (true ?state (off p)))
(rule (next ?state ?performed (off q))
  (does ?performed robot c)
  (true ?state (off r)))
(rule (next ?state ?performed (off r))
  (does ?performed robot c)
  (true ?state (off q)))

(rule (next ?state ?performed (off s))
  (does ?performed robot a)
  (true ?state (off s)))
(rule (next ?state ?performed (off s))
  (does ?performed robot b)
  (true ?state (off s)))
(rule (next ?state ?performed (off s))
  (does ?performed robot c)
  (true ?state (off s)))
(rule (next ?state ?performed (on s))
  (does ?performed robot a)
  (true ?state (on s)))
(rule (next ?state ?performed (on s))
  (does ?performed robot b)
  (true ?state (on s)))
(rule (next ?state ?performed (on s))
  (does ?performed robot c)
  (true ?state (on s)))
(rule (next ?state ?performed (off s))
  (does ?performed robot d)
  (true ?state (on s)))
(rule (next ?state ?performed (on s))
  (does ?performed robot d)
  (true ?state (off s)))

(rule (next ?state ?performed (on p))
  (does ?performed robot d)
  (true ?state (on p)))
(rule (next ?state ?performed (off p))
  (does ?performed robot d)
  (true ?state (off p)))

(rule (next ?state ?performed (on q))
  (does ?performed robot d)
  (true ?state (on q)))
(rule (next ?state ?performed (off q))
  (does ?performed robot d)
  (true ?state (off q)))

(rule (next ?state ?performed (on r))
  (does ?performed robot d)
  (true ?state (on r)))
(rule (next ?state ?performed (off r))
  (does ?performed robot d)
  (true ?state (off r)))

(rule (next ?state ?performed (step ?y))
  (true ?state (step ?x))
  (succ ?x ?y))

(rule (succ num1 num2))
(rule (succ num2 num3))
(rule (succ num3 num4))
(rule (succ num4 num5))
(rule (succ num5 num6))
(rule (succ num6 num7))
(rule (succ num7 num8))

(rule (legal robot a))
(rule (legal robot b))
(rule (legal robot c))
(rule (legal robot d))

(rule (goal ?state robot num100)
  (true ?state (on p))
  (true ?state (on q))
  (true ?state (on r))
  (true ?state (on s)))
(rule (goal ?state robot num0)
  (true ?state (off p)))
(rule (goal ?state robot num0)
  (true ?state (off q)))
(rule (goal ?state robot num0)
  (true ?state (off r)))
(rule (goal ?state robot num0)
  (true ?state (off s)))

(rule (terminal ?state)
  (true ?state (step num8)))
(rule (terminal ?state)
  (true ?state (on p))
  (true ?state (on q))
  (true ?state (on r))
  (true ?state (on s)))


(defvar *count* 0)

(defun extract (key results)
  (mapcar (lambda (result) (cdr (assoc key result))) results))

(defun to-fake-list (l)
  (if (null l)
    'nil
    `(cons ,(car l) ,(to-fake-list (cdr l)))))


(defun initial-state ()
  (to-fake-list
    (extract '?what (return-all (init ?what)))))

(defun terminalp (state)
  (raw-provable-p `(terminal ,state)))


(defun equiv-roles (move1 move2)
  (eq (car move1) (car move2)))

(defun legal-moves (state)
  (declare (ignore state))
  (let* ((individual-moves
           (loop :for move :in (return-all (legal ?role ?action))
                 :collect (cons (cdr (assoc '?role move))
                                (cdr (assoc '?action move)))))
         (joint-moves
           (apply #'map-product #'list
                  (equivalence-classes #'equiv-roles individual-moves))))
    joint-moves))


(defun roles ()
  (extract '?role (return-all (role ?role))))

(defun goal-value (state role)
  (cdr (assoc '?goal
              (raw-return-one `(goal ,state ,role ?goal)))))

(defun goal-values (state)
  (raw-return-all `(goal ,state ?role ?goal)))

(defun next-state (current-state joint-move)
  (let ((does (to-fake-list
                (loop :for (role . action) :in joint-move
                      :collect `(does ,role ,action)))))
    (to-fake-list
      (extract
        '?what
        (raw-return-all `(next ,current-state ,does ?what))))))


(defun depth-first-search (&key exhaust)
  (let ((*count* 0)
        (nodes (make-queue)))
    (enqueue (cons (initial-state) nil) nodes)
    (pprint
      (while (not (queue-empty-p nodes))
        (incf *count*)
        (destructuring-bind (state . path)
            (dequeue nodes)
          ; (format t "Searching: ~S (~D remaining)~%" state (length remaining))
          (if (and (not exhaust)
                   (eql 'num100 (goal-value state 'robot)))
            (return (list state (reverse path)))
            (let ((children
                    (when (not (terminalp state))
                      (loop :for joint-move :in (legal-moves state)
                            :collect (cons (next-state state joint-move)
                                           (cons joint-move path))))))
              (queue-append nodes children))))))
    (format t "~%Searched ~D nodes.~%" *count*)))

