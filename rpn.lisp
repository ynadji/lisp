(defpackage :rpn
  (:use :cl)
  (:import-from :nifty-funs #:split)
  (:export #:repl))

(provide :rpn)
(in-package :rpn)

(defparameter *opt* '("+" "-" "*" "/" "expt"))

(defun repl (&optional (stack nil))
  (let ((input (split (prompt-read ">") #\Space #'string)))
	(loop for x in input
	   do (cond ((string= x "quit") (return-from repl))
		    ((string= x "stack") (format t "(~{~a~^ ~})~%" stack))
		    ((string= x "clear") (setq stack nil))
		    ((find x *opt* :test #'string=)
		     (setq stack (calc stack x))
		     (format t "--> ~:d~%" (car stack)))
		    (t (setq stack (cons (read-from-string x) stack)))))
	(repl stack)))

(defun prompt-read (prompt)
  (format *query-io* "~a " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun calc (stack opt)
  (if (< (length stack) 2)
      (progn (format t "Insufficient values for ~a in (~{~a~^ ~})~%" opt stack)
	     stack)
      (cons (funcall (read-from-string opt) (cadr stack) (car stack)) (cddr stack))))