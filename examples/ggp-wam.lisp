(in-package #:bones.wam)

;;;; Rules
(setf *database* (make-database))

(push-logic-frame)

(fact (role robot))

(facts (init (off p))
       (init (off q))
       (init (off r))
       (init (off s))
       (init (step num1)))


(rule (next (on p))
  (does robot a)
  (true (off p)))
(rule (next (on q))
  (does robot a)
  (true (on q)))
(rule (next (on r))
  (does robot a)
  (true (on r)))
(rule (next (off p))
  (does robot a)
  (true (on p)))
(rule (next (off q))
  (does robot a)
  (true (off q)))
(rule (next (off r))
  (does robot a)
  (true (off r)))

(rule (next (on p))
  (does robot b)
  (true (on q)))
(rule (next (on q))
  (does robot b)
  (true (on p)))
(rule (next (on r))
  (does robot b)
  (true (on r)))
(rule (next (off p))
  (does robot b)
  (true (off q)))
(rule (next (off q))
  (does robot b)
  (true (off p)))
(rule (next (off r))
  (does robot b)
  (true (off r)))

(rule (next (on p))
  (does robot c)
  (true (on p)))
(rule (next (on q))
  (does robot c)
  (true (on r)))
(rule (next (on r))
  (does robot c)
  (true (on q)))
(rule (next (off p))
  (does robot c)
  (true (off p)))
(rule (next (off q))
  (does robot c)
  (true (off r)))
(rule (next (off r))
  (does robot c)
  (true (off q)))

(rule (next (off s))
  (does robot a)
  (true (off s)))
(rule (next (off s))
  (does robot b)
  (true (off s)))
(rule (next (off s))
  (does robot c)
  (true (off s)))
(rule (next (on s))
  (does robot a)
  (true (on s)))
(rule (next (on s))
  (does robot b)
  (true (on s)))
(rule (next (on s))
  (does robot c)
  (true (on s)))
(rule (next (off s))
  (does robot d)
  (true (on s)))
(rule (next (on s))
  (does robot d)
  (true (off s)))

(rule (next (on p))
  (does robot d)
  (true (on p)))
(rule (next (off p))
  (does robot d)
  (true (off p)))

(rule (next (on q))
  (does robot d)
  (true (on q)))
(rule (next (off q))
  (does robot d)
  (true (off q)))

(rule (next (on r))
  (does robot d)
  (true (on r)))
(rule (next (off r))
  (does robot d)
  (true (off r)))

(rule (next (step ?y))
  (true (step ?x))
  (succ ?x ?y))


(facts (succ num1 num2)
       (succ num2 num3)
       (succ num3 num4)
       (succ num4 num5)
       (succ num5 num6)
       (succ num6 num7)
       (succ num7 num8))

(facts (legal robot a)
       (legal robot b)
       (legal robot c)
       (legal robot d))


(rule (goal robot num100)
  (true (on p))
  (true (on q))
  (true (on r))
  (true (on s)))
(rule (goal robot num0)
  (true (off p)))
(rule (goal robot num0)
  (true (off q)))
(rule (goal robot num0)
  (true (off r)))
(rule (goal robot num0)
  (true (off s)))


(rule (terminal)
  (true (step num8)))
(rule (terminal)
  (true (on p))
  (true (on q))
  (true (on r))
  (true (on s)))

(finalize-logic-frame)


(defun extract (key results)
  (mapcar (lambda (result) (getf result key)) results))


(defun initial-state ()
  (extract '?what (query-all (init ?what))))

(defun terminalp ()
  (prove (terminal)))


(defun equiv-roles (move1 move2)
  (eq (car move1) (car move2)))

(defun legal-moves ()
  (let* ((individual-moves
           (query-map (lambda (move)
                        (cons (getf move '?role)
                              (getf move '?action)))
                      (legal ?role ?action)))
         (joint-moves
           (apply #'map-product #'list
                  (equivalence-classes #'equiv-roles individual-moves))))
    joint-moves))

(defun roles ()
  (extract '?role (query-all (role ?role))))

(defun goal-value (role)
  (getf (invoke-query `(goal ,role ?goal))
        '?goal))

(defun goal-values ()
  (invoke-query-all `(goal ?role ?goal)))

(defun next-state ()
  (extract '?what (query-all (next ?what))))


(defun apply-state (state)
  (push-logic-frame)
  (loop :for fact :in state
        :do (add-fact `(true ,fact)))
  (finalize-logic-frame))

(defun apply-moves (moves)
  (push-logic-frame)
  (loop :for (role . action) :in moves
        :do (add-fact `(does ,role ,action)))
  (finalize-logic-frame))


(defun clear-state ()
  (pop-logic-frame))

(defun clear-moves ()
  (pop-logic-frame))


(defvar *count* 0)


; (declaim (optimize (speed 0) (debug 3)))
;; nodes: (state . path)
(defun depth-first-search (&key exhaust)
  (let ((*count* 0)
        (nodes (make-queue)))
    (enqueue (cons (initial-state) nil) nodes)
    (pprint
      (while (not (queue-empty-p nodes))
        (incf *count*)
        (destructuring-bind (state . path)
            (dequeue nodes)
          (apply-state state)
          ; (format t "Searching: ~S (~D remaining)~%" state (length remaining))
          (if (and (not exhaust) (eql 'num100 (goal-value 'robot)))
            (progn
              (clear-state)
              (return (list state (reverse path))))
            (let ((children
                    (when (not (terminalp))
                      (loop :for joint-move :in (legal-moves)
                            :collect (prog2
                                       (apply-moves joint-move)
                                       (cons (next-state)
                                             (cons joint-move path))
                                       (clear-moves))))))
              (clear-state)
              (queue-append nodes children))))))
    (format t "~%Searched ~D nodes.~%" *count*)))

