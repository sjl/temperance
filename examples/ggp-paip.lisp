(in-package #:bones.paip)

(declaim (optimize (speed 1) (safety 3) (debug 1)))
; (declaim (optimize (speed 3) (safety 1) (debug 0)))

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

(defun legal-moves (state)
  (declare (ignore state))
  (return-all (legal ?role ?move)))

(defun roles ()
  (extract '?role (return-all (role ?role))))

(defun goal-value (state role)
  (cdr (assoc '?goal
              (raw-return-one `(goal ,state ,role ?goal)))))

(defun goal-values (state)
  (raw-return-all `(goal ,state ?role ?goal)))

(defun next-state (current-state move)
  (let ((does (to-fake-list `((does
                                ,(cdr (assoc '?role move))
                                ,(cdr (assoc '?move move)))))))
    (to-fake-list
      (extract
        '?what
        (raw-return-all `(next ,current-state ,does ?what))))))


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
  (let ((state (search-path-state search-path)))
    (and (terminalp state)
         (eql (goal-value state 'robot) 'num100))))

(defun buttons-children (search-path)
  (let ((state (search-path-state search-path))
        (path (search-path-path search-path)))
    (when (not (terminalp state))
      (loop :for move :in (legal-moves state)
            :collect (make-search-path :state (next-state state move)
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

; (sb-sprof:with-profiling
;     (:report :flat
;      :sample-interval 0.001
;      :loop nil)
;   (dfs-exhaust)
;   )
