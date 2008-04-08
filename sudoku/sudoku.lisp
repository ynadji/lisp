(defpackage :sudoku
  (:use :cl :config-parser)
  (:export #:backtracking))

(provide :sudoku)
(in-package :sudoku)

(defvar *full-list* '(1 2 3 4 5 6 7 8 9))
(defvar *node-count* 0)

(defun backtracking (file &optional (outfile "./solution.txt"))
  (let ((puzzle (config-parser:sudoku-config-parse file)))
  (if (puzzle-solved? (backtrack-next -1 0 puzzle))
      (config-parser:sudoku-config-dump puzzle outfile)
      (print "FAILED"))
  (format t "Nodes expanded: ~d~%" *node-count*)
  (setq *node-count* 0)))

;; not very lisp-y =(
;; recursive portion of backtracking, iterates through the lists and evaluates each value as necessary
(defun backtrack-next (x y puzzle)
  (incf *node-count*)
  (if (puzzle-solved? puzzle)
    puzzle
    (progn
      (incf x)
      (when (= 9 x)
	(setf x 0)
	(incf y))
      (if (not (null (nth x (nth y puzzle))))
	(backtrack-next x y puzzle)
	(let ((missing (possible-vals x y puzzle)))
	  (when missing
	    (dolist (val missing nil)
	      (setf (nth x (nth y puzzle)) val)
	      (backtrack-next x y puzzle)
	      (when (puzzle-solved? puzzle) (return-from backtrack-next puzzle))
	      (setf (nth x (nth y puzzle)) nil))))))))

;;; return possible values for the respective node
(defun possible-vals (x y puzzle)
  (possible-vals-asst
   (union
    (get-square (pick-square x y) puzzle)
    (union (get-row y puzzle)
	   (get-col x puzzle)))))

(defun possible-vals-asst (vals)
  (loop for x from 1 to 9
	with result = nil
	finally (return result)
	do (unless (find x vals) (push x result))))

;;; puzzle-solved?: given a sudoku puzzle, determines if it is solved
(defun puzzle-solved? (puzzle)
  (loop for x from 0 to 8
	always (unit-good? (get-row x puzzle))
	always (unit-good? (get-col x puzzle))
	always (unit-good? (get-square x puzzle))))

;;; unit-good?: given a list (representing either a 3x3 square, a row or column) determines if it's finished
(defun unit-good? (unit)
  (let ((temp-list *full-list*))
    (loop for x in unit
	  always (member x temp-list)
	  do (setf temp-list (remove x temp-list)))))

;;; get-row: returns a row based on the respective integer
(defun get-row (row-num puzzle)
  (nth row-num puzzle))

;;; get-col: returns a column based on the respective integer
(defun get-col (col-num puzzle)
  (loop for x in puzzle
	collect (nth col-num x)))

;;; pick-square: given (x,y) coordinates, picks appropriate unit-square
(defun pick-square (x y)
  (cond
    ((> x 5)
     (cond
       ((> y 5) 8)
       ((> y 2) 5)
       (t 2)))
    ((> x 2)
     (cond
       ((> y 5) 7)
       ((> y 2) 4)
       (t 1)))
    (t
     (cond
       ((> y 5) 6)
       ((> y 2) 3)
       (t 0)))))

;;; get-square: integer representing one 3x3 square
(defun get-square (square-num puzzle)
  (cond
    ((= square-num 0) (get-square-aid 0 0 puzzle))
    ((= square-num 1) (get-square-aid 3 0 puzzle))
    ((= square-num 2) (get-square-aid 6 0 puzzle))
    ((= square-num 3) (get-square-aid 0 3 puzzle))
    ((= square-num 4) (get-square-aid 3 3 puzzle))
    ((= square-num 5) (get-square-aid 6 3 puzzle))
    ((= square-num 6) (get-square-aid 0 6 puzzle))
    ((= square-num 7) (get-square-aid 3 6 puzzle))
    ((= square-num 8) (get-square-aid 6 6 puzzle))))

(defun get-square-aid (x y puzzle)
  (list
   (nth x (nth y puzzle)) (nth (+ x 1) (nth y puzzle)) (nth (+ x 2) (nth y puzzle))
   (nth x (nth (+ y 1) puzzle)) (nth (+ x 1) (nth (+ 1 y) puzzle)) (nth (+ x 2) (nth (+ y 1) puzzle))
   (nth x (nth (+ y 2) puzzle)) (nth (+ x 1) (nth (+ y 2) puzzle)) (nth (+ x 2) (nth (+ y 2) puzzle))))
