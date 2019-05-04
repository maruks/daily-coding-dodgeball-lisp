(fiasco:define-test-package :dodgeball-tests
  (:use :dodgeball :check-it))

(in-package :dodgeball-tests)

(defun range (end)
  (loop :for i :below end :collect i))

(defun test-gen (max-size)
  (generator
   (chain ((n (integer 2 max-size)))
	  (generator (list
                      (list (integer 0 (1- n)) :min-length 0 :max-length (min 1 (floor n 2)))
                      :length n)))))

(defun graph (ns test-data result)
  (if ns
      (let* ((n (car ns))
	     (players (cdr (nth n test-data)))
	     (assigned (reduce (lambda (a e) (if (member n (cdr e)) (cons (car e) a) a)) test-data :initial-value nil))
	     (new (remove n (remove-duplicates (append players assigned)))))
	(graph (cdr ns) test-data (acons n new result)))
      result))

(defun test->graph (test-data)
  (let ((players (reverse (range (length test-data)))))
    (graph players (pairlis players test-data) nil)))

(defparameter *input-1* '((0 . (3))
			  (1 . (2))
			  (2 . (1 4))
			  (3 . (0 4 5))
			  (4 . (2 3))
			  (5 . (3))))

(defparameter *input-2* '((0 . (3))
			  (1 . (2))
			  (2 . (1 3 4))
			  (3 . (0 2 4 5))
			  (4 . (2 3))
			  (5 . (3))))

(defparameter *input-3* '((0 . (3))
			  (1 . (2))
			  (2 . (1 4))
			  (3 . (0 4 5))
			  (4 . (2 3))
			  (5 . (3))
			  (6 . ())
			  (7 . ())))

(defun all-players-compatible? (team graph)
  (every (lambda (player) (null (intersection team (cdr (assoc player graph))))) team))

(defun all-players-present? (all-players team-a team-b)
  (let ((players-in-result (append team-a team-b)))
    (and (null (set-difference all-players players-in-result))
         (null (set-difference players-in-result all-players))
         (null (intersection team-a team-b)))))

(defun is-result-valid? (result graph)
  (or (null result)
      (let ((team-a (cdr (assoc :A result)))
            (team-b (cdr (assoc :B result)))
            (all-players (mapcar #'car graph)))
        (and (all-players-present? all-players team-a team-b)
             (every #'identity (mapcar (lambda (team) (all-players-compatible? team graph)) (list team-a team-b)))))))

(deftest is-result-valid-test ()
  (is (is-result-valid? nil *input-1*))
  (is (is-result-valid? '((:A . (0 1 4 5)) (:B . (2 3))) *input-1*))
  (is (is-result-valid? '((:A . (0 1 4 5)) (:B . (2 3 6 7))) *input-3*))
  (is (is-result-valid? '((:A . (0 1 4 5 6)) (:B . (2 3 7))) *input-3*))
  (is (is-result-valid? '((:A . (0 1 4 5 6 7)) (:B . (2 3))) *input-3*))
  (is (not (is-result-valid? '((:A . (0 1 4 5 6 7)) (:B . (2 3 7))) *input-3*)))
  (is (not (is-result-valid? '((:A . (0 1 4 5 6 7)) (:B . (2 3 6))) *input-3*)))
  (is (not (is-result-valid? '((:A . (1 4 5 6 7)) (:B . (2 3 0))) *input-3*)))
  (is (not (is-result-valid? '((:A . (0 1 5 6 7)) (:B . (2 3 4))) *input-3*)))
  (is (not (is-result-valid? '((:A . (0 1 4 6 7)) (:B . (2 3 5))) *input-3*)))
  (is (not (is-result-valid? '((:A . (0 4 5 6 7)) (:B . (2 3 1))) *input-3*)))
  (is (not (is-result-valid? '((:A . (0 1 5 6 7)) (:B . (2 3 4))) *input-3*))))

(deftest split-examples-test ()
  (is (equal '((:A . (0 1 4 5)) (:B . (2 3))) (split *input-1*)))
  (is (null (split *input-2*)))
  (is (equal '((:A . (0 1 4 5)) (:B . (2 3 6 7))) (split *input-3*))))

(deftest split-gen-test ()
  (let ((*num-trials* 1000))
    (is (check-it (test-gen 64)
                  (lambda (x) (let ((input (test->graph x)))
                                (is (is-result-valid? (split input) input))))))))
