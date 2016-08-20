(in-package #:temperance-test.circle)

(defmacro is-circle-contents (circle values)
  `(is (equal ,values
              (circle-to-list ,circle))))


(define-test empty-circles
  (is (circle-empty-p (make-empty-circle)))
  (is (circle-empty-p (make-circle-with nil)))
  (is (not (circle-empty-p (make-circle-with (list 1))))))

(define-test making-circle-with
  (is-circle-contents
    (make-circle-with (list))
    nil)
  (is-circle-contents
    (make-circle-with (list 1 2 3))
    (list 1 2 3))
  (is-circle-contents
    (make-circle-with '(foo))
    (list 'foo))
  (is-circle-contents
    (make-circle-with '((foo)))
    (list (list 'foo))))


(define-test prepending
  (let ((c (make-empty-circle)))
    (is-circle-contents c nil)

    (circle-prepend c (list 1))
    (is-circle-contents c '(1))

    (circle-prepend c (list 2 3))
    (is-circle-contents c '(2 3 1))

    (circle-prepend c nil)
    (is-circle-contents c '(2 3 1))))

(define-test appending
  (let ((c (make-empty-circle)))
    (is-circle-contents c nil)

    (circle-append c (list 1))
    (is-circle-contents c '(1))

    (circle-append c (list 2 3))
    (is-circle-contents c '(1 2 3))

    (circle-append c nil)
    (is-circle-contents c '(1 2 3))))

(define-test appending-and-prepending
  (let ((c (make-empty-circle)))
    (is-circle-contents c nil)

    (circle-append c (list 1))
    (is-circle-contents c '(1))

    (circle-prepend c (list 'a 'b))
    (is-circle-contents c '(a b 1))

    (circle-append c (list 'p 'q))
    (is-circle-contents c '(a b 1 p q))))


(define-test moving-forward
  (let ((c (make-circle-with (list 1 2 3 4))))
    (is (equal
          '(1 2 3 4)
          (loop :for node = (circle-forward c) :then (circle-forward node)
                :while node
                :collect (circle-value node))))))

(define-test moving-backward
  (let ((c (make-circle-with (list 1 2 3 4))))
    (is (equal
          '(4 3 2 1)
          (loop :for node = (circle-backward c) :then (circle-backward node)
                :while node
                :collect (circle-value node))))))


(define-test rotating
  (let ((c (make-circle-with (list 1 2 3 4))))
    (is-circle-contents (circle-rotate c 0)
                        '(1 2 3 4))
    (is-circle-contents (circle-rotate c 1)
                        '(1 2 3 4))
    (is-circle-contents (circle-rotate c 2)
                        '(2 3 4 1))
    (is-circle-contents (circle-rotate c 3)
                        '(3 4 1 2))
    (is-circle-contents (circle-rotate c 4)
                        '(4 1 2 3))
    (is-circle-contents (circle-rotate c 5)
                        '(1 2 3 4))
    (is-circle-contents (circle-rotate c -1)
                        '(4 1 2 3))
    (is-circle-contents (circle-rotate c -2)
                        '(3 4 1 2))
    (is-circle-contents (circle-rotate c -3)
                        '(2 3 4 1))
    (is-circle-contents (circle-rotate c -4)
                        '(1 2 3 4))
    (is-circle-contents (circle-rotate (circle-rotate c 2) 0)
                        '(2 3 4 1))
    (is-circle-contents (circle-rotate (circle-rotate c 2) 1)
                        '(3 4 1 2))
    (is-circle-contents (circle-rotate (circle-rotate c 2) 2)
                        '(4 1 2 3))
    (is-circle-contents (circle-rotate (circle-rotate c 2) -2)
                        '(1 2 3 4))
    (is-circle-contents (circle-rotate (circle-rotate c 3) -1)
                        '(2 3 4 1))))


(define-test retrieving-nth
  (let* ((data (list 'a 'b 'c 'd))
         (c (make-circle-with data)))
    (loop :for i :from 0 :below 4
          :for v :in data
          :do (is (eql v (circle-value (circle-nth c i)))))))


(define-test inserting-before
  (let ((c (make-circle-with (list 1 2 3))))
    (circle-insert-before c 'a)
    (is-circle-contents c '(1 2 3 a))

    (circle-insert-before (circle-nth c 0) 'b)
    (is-circle-contents c '(b 1 2 3 a))

    (circle-insert-before (circle-nth c 1) 'c)
    (is-circle-contents c '(b c 1 2 3 a))

    (circle-insert-before (circle-nth c 2) 'd)
    (is-circle-contents c '(b c d 1 2 3 a))

    (circle-insert-before (circle-nth c -1) 'e)
    (is-circle-contents c '(b c d 1 2 3 e a))))

(define-test inserting-after
  (let ((c (make-circle-with (list 1 2 3))))
    (circle-insert-after c 'a)
    (is-circle-contents c '(a 1 2 3))

    (circle-insert-after (circle-nth c 0) 'b)
    (is-circle-contents c '(a b 1 2 3))

    (circle-insert-after (circle-nth c 1) 'c)
    (is-circle-contents c '(a b c 1 2 3))

    (circle-insert-after (circle-nth c 2) 'd)
    (is-circle-contents c '(a b c d 1 2 3))

    (circle-insert-after (circle-nth c -1) 'x)
    (is-circle-contents c '(a b c d 1 2 3 x))))


(define-test checking-sentinel
  (let ((c (make-circle-with (list 1 2 3))))
    (is (circle-sentinel-p c))
    (is (not (circle-sentinel-p (circle-nth c 0))))
    (is (not (circle-sentinel-p (circle-nth c 1))))
    (is (not (circle-sentinel-p (circle-nth c 2))))
    (is (circle-sentinel-p (circle-nth c 3))))
  (is (circle-sentinel-p (make-empty-circle)))
  (is (circle-sentinel-p (circle-nth (make-empty-circle) 0)))
  (is (circle-sentinel-p (circle-nth (make-empty-circle) -1))))


(define-test removing
  (let ((c (make-circle-with (list 1 2 3))))
    (signals simple-error (circle-remove c))
    (is-circle-contents c '(1 2 3))

    (circle-remove (circle-nth c 0))
    (is-circle-contents c '(2 3))

    (circle-remove (circle-nth c 1))
    (is-circle-contents c '(2))

    (circle-remove (circle-nth c 0))
    (is-circle-contents c '())))

(define-test removing-backward
  (let ((c (make-circle-with (list 1 2 3 4 5 6))))
    (is-circle-contents c '(1 2 3 4 5 6))

    (is-circle-contents (circle-backward-remove (circle-nth c 1))
                        '(1 3 4 5 6))

    (is (not (circle-backward-remove (circle-nth c 0))))
    (is-circle-contents c '(3 4 5 6))

    (is-circle-contents (circle-backward-remove (circle-nth c -1))
                        '(5 3 4))

    (is-circle-contents c '(3 4 5))))

(define-test removing-forward
  (let ((c (make-circle-with (list 1 2 3 4 5 6))))
    (is-circle-contents c '(1 2 3 4 5 6))

    (is-circle-contents (circle-forward-remove (circle-nth c 1))
                        '(3 4 5 6 1))

    (is (not (circle-forward-remove (circle-nth c -1))))
    (is-circle-contents c '(1 3 4 5))))


(define-test replacing
  (let ((c (make-circle-with (list 1 2 3 4 5 6))))
    (is-circle-contents c '(1 2 3 4 5 6))

    (circle-replace (circle-nth c 0) 'foo)
    (is-circle-contents c '(foo 2 3 4 5 6))

    (circle-replace (circle-nth c 0) 'bar)
    (is-circle-contents c '(bar 2 3 4 5 6))

    (circle-replace (circle-nth c 1) 'a)
    (is-circle-contents c '(bar a 3 4 5 6))

    (circle-replace (circle-nth c 2) 'b)
    (is-circle-contents c '(bar a b 4 5 6))

    (circle-replace (circle-nth c -1) 'c)
    (is-circle-contents c '(bar a b 4 5 c))))

(define-test replacing-backward
  (let ((c (make-circle-with (list 1 2 3 4 5 6))))
    (is-circle-contents c '(1 2 3 4 5 6))

    (is-circle-contents (circle-backward-replace (circle-nth c 1) 'foo)
                        '(1 foo 3 4 5 6))

    (is-circle-contents (circle-backward-replace (circle-nth c 1) 'bar)
                        '(1 bar 3 4 5 6))

    (is-circle-contents (circle-backward-replace (circle-nth c 2) 'a)
                        '(bar a 4 5 6 1))

    (is (not (circle-backward-replace (circle-nth c 0) 'dogs)))
    (is-circle-contents c '(dogs bar a 4 5 6))))

(define-test replacing-forward
  (let ((c (make-circle-with (list 1 2 3 4 5 6))))
    (is-circle-contents c '(1 2 3 4 5 6))

    (is-circle-contents (circle-forward-replace (circle-nth c 1) 'foo)
                        '(3 4 5 6 1 foo))

    (is-circle-contents (circle-forward-replace (circle-nth c 1) 'bar)
                        '(3 4 5 6 1 bar))

    (is (not (circle-forward-replace (circle-nth c -1) 'cats)))
    (is-circle-contents c '(1 bar 3 4 5 cats))))


(define-test splicing
  (let ((c (make-circle-with (list 1 2 3 4 5 6))))
    (is-circle-contents c '(1 2 3 4 5 6))

    (circle-splice (circle-nth c 0) (list 'a 'b))
    (is-circle-contents c '(a b 2 3 4 5 6))

    (circle-splice (circle-nth c 1) (list 'c))
    (is-circle-contents c '(a c 2 3 4 5 6))

    (circle-splice (circle-nth c -1) (list 'dogs 'cats))
    (is-circle-contents c '(a c 2 3 4 5 dogs cats))

    (circle-splice (circle-nth c 3) nil)
    (is-circle-contents c '(a c 2 4 5 dogs cats))))

(define-test splicing-backward
  (let ((c (make-circle-with (list 1 2 3 4 5 6))))
    (is-circle-contents c '(1 2 3 4 5 6))

    (is-circle-contents (circle-backward-splice (circle-nth c 2) '(a b))
                        '(2 a b 4 5 6 1))

    (is-circle-contents (circle-backward-splice (circle-nth c -1) '())
                        '(5 1 2 a b 4))

    (is (not (circle-backward-splice (circle-nth c 0) '(first second))))
    (is-circle-contents c '(first second 2 a b 4 5))))

(define-test splicing-forward
  (let ((c (make-circle-with (list 1 2 3 4 5 6))))
    (is-circle-contents c '(1 2 3 4 5 6))

    (is-circle-contents (circle-forward-splice (circle-nth c 0) '(a b))
                        '(2 3 4 5 6 a b))

    (is-circle-contents (circle-forward-splice (circle-nth c 1) '())
                        '(2 3 4 5 6 a))

    (is (not (circle-forward-splice (circle-nth c -1) '(last))))
    (is-circle-contents c '(a 2 3 4 5 last))))
