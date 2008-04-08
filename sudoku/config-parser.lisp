(defpackage :config-parser
  (:use :cl)
  (:export #:sudoku-config-parse)
  (:export #:sudoku-config-dump))

(provide :config-parser)
(in-package :config-parser)

(defun c2i (c)
  (if (not (null c))
      (let ((h (char-int c)))
	; if char falls within this boundry, it's an integer (0-9)
	; returns integer
	(cond ((and (> h 47) (< h 58)) (- h 48))
	      ; if it's a space, return -1
	      ; can't use NIL b/c of DO loop constraint
	      ((= h 32) -1)
	      (t c)))))

(defun sudoku-config-parse (file)
  (let ((final NIL)
	(accum NIL)
	(col 1))
    (with-open-file (in file)
      (do ((val (c2i (read-char in nil))
		(c2i (read-char in nil))))
	  ((null val))
	(if (numberp val)
	    (progn (if (equal 0 (mod col 10))
		       (progn (setf final (append final (list accum)))
			      (setf accum NIL)
			      (setf col 1)))
		   (setf accum (append accum (list
					      ; if the value returned by c2i is -1
					      ; it was a space, so set it to NIL
					      (if (= val -1)
						 nil
						 val))))
		   (incf col)))))
    (setf final (append final (list accum)))
    final))

(defun print-conf-line (line stream)
  (write-char (digit-char (first line)) stream)
  (if (null (rest line))
      nil
      (progn (write-char #\, stream)
	     (print-conf-line (rest line) stream))))
  
(defun sudoku-config-dump (puzzle file)
  ; if the puzzle is nil, everything has been written out already
  (if (null puzzle)
      nil
      (progn (with-open-file (out file :direction :output :if-exists :append :if-does-not-exist :create)
	       (print-conf-line (first puzzle) out)
	       (fresh-line out))
	     (sudoku-config-dump (rest puzzle) file))))