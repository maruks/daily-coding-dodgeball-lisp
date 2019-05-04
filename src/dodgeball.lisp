(defpackage :dodgeball
  (:use :cl)
  (:export :split))

(in-package :dodgeball)

(defparameter *teams* '(:A :B))

(defun opp-team (team)
  (car (remove team *teams*)))

(defun process (queue teams graph)
  (if queue
      (let* ((player (caar queue))
	     (team (cdar queue))
	     (assigned (cdr (assoc player teams))))
	(cond ((null assigned) (let* ((team-2 (opp-team team))
				      (players-2 (cdr (assoc player graph)))
				      (teams-2 (make-list (length players-2) :initial-element team-2))
				      (next-queue (pairlis players-2 teams-2 (cdr queue)))
				      (next-teams (acons player team teams)))
				 (process next-queue next-teams graph)))
	      ((eq assigned team) (process (cdr queue) teams graph))))
      (let* ((unassigned-players (set-difference (mapcar #'car graph) (mapcar #'car teams))))
	(if unassigned-players
	    (let* ((team-1 (car *teams*))
		   (team-1-size (length (remove-if-not (lambda (e) (eq e team-1)) teams :key #'cdr)))
		   (smallest-team (if (< team-1-size (- (length teams) team-1-size)) team-1 (cadr *teams*))))
	      (process (acons (car unassigned-players) smallest-team nil) teams graph))
	    (let ((reducer (lambda (a e) (acons e (sort (mapcar #'car (remove-if-not (lambda (e2) (eq e (cdr e2))) teams)) #'<) a))))
	      (reduce reducer (reverse *teams*) :initial-value nil))))))

(defun split (graph)
  (process (acons (caar graph) (car *teams*) nil) nil graph))
