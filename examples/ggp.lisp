(in-package #:bones.wam)

(declaim (optimize (speed 1) (safety 3) (debug 1)))
; (declaim (optimize (speed 3) (safety 1) (debug 0)))

(defparameter *d* (make-database))

(with-database *d*
  (rules ((member :thing (cons :thing :rest)))
         ((member :thing (cons :other :rest))
          (member :thing :rest)))

  (rule (true :state :thing)
        (member :thing :state))

  (rule (does :performed :role :move)
        (member (does :role :move) :performed))

  (fact (role robot))

  (facts (init (off p))
         (init (off q))
         (init (off r))
         (init (off s))
         (init (step num1))))

(with-database *d*
  (rules ((next :state :performed (on p))
          (does :performed robot a)
          (true :state (off p)))
         ((next :state :performed (on q))
          (does :performed robot a)
          (true :state (on q)))
         ((next :state :performed (on r))
          (does :performed robot a)
          (true :state (on r)))
         ((next :state :performed (off p))
          (does :performed robot a)
          (true :state (on p)))
         ((next :state :performed (off q))
          (does :performed robot a)
          (true :state (off q)))
         ((next :state :performed (off r))
          (does :performed robot a)
          (true :state (off r)))

         ((next :state :performed (on p))
          (does :performed robot b)
          (true :state (on q)))
         ((next :state :performed (on q))
          (does :performed robot b)
          (true :state (on p)))
         ((next :state :performed (on r))
          (does :performed robot b)
          (true :state (on r)))
         ((next :state :performed (off p))
          (does :performed robot b)
          (true :state (off q)))
         ((next :state :performed (off q))
          (does :performed robot b)
          (true :state (off p)))
         ((next :state :performed (off r))
          (does :performed robot b)
          (true :state (off r)))

         ((next :state :performed (on p))
          (does :performed robot c)
          (true :state (on p)))
         ((next :state :performed (on q))
          (does :performed robot c)
          (true :state (on r)))
         ((next :state :performed (on r))
          (does :performed robot c)
          (true :state (on q)))
         ((next :state :performed (off p))
          (does :performed robot c)
          (true :state (off p)))
         ((next :state :performed (off q))
          (does :performed robot c)
          (true :state (off r)))
         ((next :state :performed (off r))
          (does :performed robot c)
          (true :state (off q)))

         ((next :state :performed (off s))
          (does :performed robot a)
          (true :state (off s)))
         ((next :state :performed (off s))
          (does :performed robot b)
          (true :state (off s)))
         ((next :state :performed (off s))
          (does :performed robot c)
          (true :state (off s)))
         ((next :state :performed (on s))
          (does :performed robot a)
          (true :state (on s)))
         ((next :state :performed (on s))
          (does :performed robot b)
          (true :state (on s)))
         ((next :state :performed (on s))
          (does :performed robot c)
          (true :state (on s)))
         ((next :state :performed (off s))
          (does :performed robot d)
          (true :state (on s)))
         ((next :state :performed (on s))
          (does :performed robot d)
          (true :state (off s)))

         ((next :state :performed (on p))
          (does :performed robot d)
          (true :state (on p)))
         ((next :state :performed (off p))
          (does :performed robot d)
          (true :state (off p)))

         ((next :state :performed (on q))
          (does :performed robot d)
          (true :state (on q)))
         ((next :state :performed (off q))
          (does :performed robot d)
          (true :state (off q)))

         ((next :state :performed (on r))
          (does :performed robot d)
          (true :state (on r)))
         ((next :state :performed (off r))
          (does :performed robot d)
          (true :state (off r)))

         ((next :state :performed (step :y))
          (true :state (step :x))
          (succ :x :y))))

(with-database *d*
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
         (legal robot d)))

(with-database *d*
  (rules ((goal :state robot num100)
          (true :state (on p))
          (true :state (on q))
          (true :state (on r))
          (true :state (on s))
          )
         ((goal :state robot num0)
          (true :state (off p)))
         ((goal :state robot num0)
          (true :state (off q)))
         ((goal :state robot num0)
          (true :state (off r)))
         ((goal :state robot num0)
          (true :state (off s)))
         )

  (rules ((terminal :state)
          (true :state (step num8)))
         ((terminal :state)
          (true :state (on p))
          (true :state (on q))
          (true :state (on r))
          (true :state (on s))
          )))


(defun extract (key results)
  (mapcar (lambda (result) (getf result key)) results))

(defun to-fake-list (l)
  (if (null l)
    'nil
    `(cons ,(car l) ,(to-fake-list (cdr l)))))

(defun initial-state ()
  (to-fake-list
    (with-database *d*
      (extract :what (return-all (init :what))))))

(defun terminalp (state)
  (with-database *d*
    (perform-prove `((terminal ,state)))))

(defun legal-moves (state)
  (declare (ignore state))
  (with-database *d*
    (return-all (legal :role :move))))

(defun roles ()
  (with-database *d*
    (extract :role (return-all (role :role)))))

(defun goal-value (state role)
  (with-database *d*
    (getf (perform-return `((goal ,state ,role :goal)) :one) :goal)))

(defun goal-values (state)
  (with-database *d*
    (perform-return `((goal ,state :role :goal)) :all)))

(defun next-state (current-state move)
  (let ((does (to-fake-list `((does
                                ,(getf move :role)
                                ,(getf move :move))))))
    (with-database *d*
      (to-fake-list
        (extract :what
               (perform-return `((next ,current-state ,does :what)) :all))))))


(defstruct search-path state (path nil) (previous nil))

(defun tree-search (states goal-p children combine)
  (labels
      ((recur (states)
         (if (null states)
           nil
           (destructuring-bind (state . remaining) states
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
  (tree-search (list (make-search-path :state (initial-state)))
               #'never
               #'buttons-children
               #'append))

(defun bfs ()
  (tree-search (list (make-search-path :state (initial-state)))
               #'buttons-goal-p
               #'buttons-children
               (lambda (x y)
                 (append y x))))

(sb-sprof:with-profiling
    (:report :flat
     :sample-interval 0.001
     :loop nil)
  (dfs-exhaust)
  )
