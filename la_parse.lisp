;(defpackage :la-parse
;  (:use :cl)
;  (:export #:la-parse)
;  (:export #:la-dump))

;;; Parses the text file containing the
;;; requests of the retreat individuals
(defun la-parse (file)
  (let ((final nil))
    (with-open-file (in file)
      (do ((person (process-line (read-line in nil nil nil))
		   (process-line (read-line in nil nil nil))))
	  ((null person))
	(setf final (append final (list person)))))
    final))
      
;;; Parses a line from the input file in the format:
;;; ("name" (# # # # # #) "gname"), gname will be filled
;;; in by the main program
(defun process-line (line)
  ; if the line is EOF, return nil to break the do loop
  (if (null line) nil
  (let ((newstr (loop for i = 0 then (1+ j)
		      as j = (position #\, line :start i)
		      collect (subseq line i j)
		      while j)))
    (append (cons (first newstr) (cons (mapcar #'parse-integer (rest newstr)) '(nil)))))))

;;; Dumps the solution out to a file
;;; or prints it to screen
(defun la-dump (soln)
  (dolist (person soln)
    (format t "~a:~@T~a~%" (first (last person)) (first person))))

;(provide :la-parse)
