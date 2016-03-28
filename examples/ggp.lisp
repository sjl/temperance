(in-package #:bones.paip)

;;;; Games
(defun dont-press-the-button ()
  (clear-db)
  (fact role you)

  (fact init (button off))
  (fact init (turn 0))

  (fact always) ; work around broken gamestepper

  (rule (legal you press) (always))
  (rule (legal you wait) (always))

  (rule (next (button on))
        (does you press))

  (rule (next (button off))
        (does you wait))

  (rule (next (turn ?x))
        (true (turn ?current))
        (succ ?current ?x))

  (rule (terminal)
        (true (button on)))

  (rule (terminal)
        (true (turn 4)))

  (rule (goal you 100)
        (true (button off)))

  (rule (goal you 0)
        (true (button on)))

  (fact succ 0 1)
  (fact succ 1 2)
  (fact succ 2 3)
  (fact succ 3 4))

(defun tic-tac-toe ()
  (clear-db)

  (fact role xplayer)
  (fact role oplayer)

  (fact index 1)
  (fact index 2)
  (fact index 3)
  (rule (base (cell ?x ?y b)) (index ?x) (index ?y))
  (rule (base (cell ?x ?y x)) (index ?x) (index ?y))
  (rule (base (cell ?x ?y o)) (index ?x) (index ?y))
  (rule (base (control ?p)) (role ?p))

  (rule (input ?p (mark ?x ?y)) (index ?x) (index ?y) (role ?p))
  (rule (input ?p noop) (role ?p))

  (fact init (cell 1 1 b))
  (fact init (cell 1 2 b))
  (fact init (cell 1 3 b))
  (fact init (cell 2 1 b))
  (fact init (cell 2 2 b))
  (fact init (cell 2 3 b))
  (fact init (cell 3 1 b))
  (fact init (cell 3 2 b))
  (fact init (cell 3 3 b))
  (fact init (control xplayer))

  (rule (next (cell ?m ?n x))
        (does xplayer (mark ?m ?n))
        (true (cell ?m ?n b)))

  (rule (next (cell ?m ?n o))
        (does oplayer (mark ?m ?n))
        (true (cell ?m ?n b)))

  (rule (next (cell ?m ?n ?w))
        (true (cell ?m ?n ?w))
        (distinct ?w b))

  (rule (next (cell ?m ?n b))
        (does ?w (mark ?j ?k))
        (true (cell ?m ?n b))
        (or (distinct ?m ?j) (distinct ?n ?k)))

  (rule (next (control xplayer))
        (true (control oplayer)))

  (rule (next (control oplayer))
        (true (control xplayer)))

  (rule (row ?m ?x)
        (true (cell ?m 1 ?x))
        (true (cell ?m 2 ?x))
        (true (cell ?m 3 ?x)))

  (rule (column ?n ?x)
        (true (cell 1 ?n ?x))
        (true (cell 2 ?n ?x))
        (true (cell 3 ?n ?x)))

  (rule (diagonal ?x)
        (true (cell 1 1 ?x))
        (true (cell 2 2 ?x))
        (true (cell 3 3 ?x)))

  (rule (diagonal ?x)
        (true (cell 1 3 ?x))
        (true (cell 2 2 ?x))
        (true (cell 3 1 ?x)))

  (rule (line ?x) (row ?m ?x))
  (rule (line ?x) (column ?m ?x))
  (rule (line ?x) (diagonal ?x))

  (rule (open)
        (true (cell ?m ?n b)))

  (rule (legal ?w (mark ?x ?y))
        (true (cell ?x ?y b))
        (true (control ?w)))

  (rule (legal xplayer noop)
        (true (control oplayer)))

  (rule (legal oplayer noop)
        (true (control xplayer)))

  (rule (goal xplayer 100)
        (line x))

  (rule (goal xplayer 50)
        (not (line x))
        (not (line o))
        (not open))

  (rule (goal xplayer 0)
        (line o))

  (rule (goal oplayer 100)
        (line o))

  (rule (goal oplayer 50)
        (not (line x))
        (not (line o))
        (not open))

  (rule (goal oplayer 0)
        (line x))

  (rule (terminal)
        (line x))

  (rule (terminal)
        (line o))

  (rule (terminal)
        (not open)))


;;;; GGP
(defun random-elt (seq)
  (elt seq (random (length seq))))

(defun clear-state! ()
  (clear-predicate 'true)
  (clear-predicate 'does))

(defun extract-results (bindings)
  (loop :for binding-list :in bindings
        :collect (cdar binding-list)))

(defun build-init! ()
  (let ((initial-state (return-all (init ?state))))
    (loop :for state :in (extract-results initial-state)
          :do (add-fact `(true ,state))))
  (values))

(defun legal-moves (role)
  (extract-results (return-all-for `((legal ,role ?move)))))

(defun make-move! (role move)
  (add-fact `(does ,role ,move)))

(defun advance-state! ()
  (let ((next-facts (extract-results (return-all-for '((next ?state))))))
    (clear-state!)
    (mapc (lambda (n) (add-fact `(true ,n))) next-facts)))

(defun roles ()
  (extract-results (return-all (role ?r))))

(defun random-moves ()
  (loop :for role :in (roles)
        :collect (cons role (random-elt (legal-moves role)))))

(defun make-random-moves! ()
  (loop :for (role . move) :in (random-moves)
        :do (make-move! role move)))

(defun terminal-p ()
  (provable-p (terminal)))

(defun goals ()
  (loop :for result :in (return-all (goal ?role ?val))
        :collect (cons (cdr (assoc '?role result))
                       (cdr (assoc '?val result)))))

(defun depth-charge! ()
  (if (terminal-p)
    (goals)
    (progn
      (make-random-moves!)
      (advance-state!)
      (depth-charge!))))

(defun fresh-depth-charge! ()
  (progn (clear-state!)
         (build-init!)
         (depth-charge!)))


;;;; Run
(dont-press-the-button)
(tic-tac-toe)
(clear-state!)
(build-init!)
(advance-state!)
(make-random-moves!)
(query-all (next ?x))
(query-all (true ?x))
(query-all (does ?r ?m))

(fresh-depth-charge!)
(time
  (dotimes (i 10)
    (fresh-depth-charge!)))
