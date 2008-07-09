(defpackage :onlisp
  (:use :cl)
  (:export #:flatten)
  (:export #:after)
  (:export #:before)
  (:export #:memoize))

(provide :onlisp)
(in-package :onlisp)

;;;; Stuff from On Lisp -- AKA useful

;;; Nice, basic functions
(defun longer (x y)
  (labels ((compare (x y)
	     (and (consp x)
		  (or (null y)
		      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
	(compare x y)
	(> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
	     (let ((rest (nthcdr n source)))
	       (if (consp rest)
		   (rec rest (cons (subseq source 0 n) acc))
		   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;;; Doubly-recursive list utilities
(defun flatten (x)
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
	     (cond ((null tree) (nreverse acc))
		   ((consp (car tree))
		    (rec (cdr tree)
			 (cons (rec (car tree) nil) acc)))
		   (t (rec (cdr tree)
			   (if (funcall test (car tree))
			       acc
			       (cons (car tree) acc)))))))
    (rec tree nil)))

;;; Functions which search lists
(defun find2 (fn lst)
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
	(if val
	    (values (car lst) val)
	    (find2 fn (cdr lst))))))

; checks if x occurs before y in lst
(defun before (x y lst &key (test #'eql))
  (and lst
       (let ((first (car lst)))
	 (cond ((funcall test y first) nil)
	       ((funcall test x first) lst)
	       (t (before x y (cdr lst) :test test))))))

; checks if x occurs after y in lst
(defun after (x y lst &key (test #'eql))
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

; checks for dupes
(defun duplicate (obj lst &key (test #'eql))
  (member obj (cdr (member obj lst :test test))
	  :test test))

(defun split-if (fn lst)
  (let ((acc nil))
    (do ((src lst (cdr src)))
	((or (null src) (funcall fn (car src)))
	 (values (nreverse acc) src))
      (push (car src) acc))))

;;; Compare elements against one another

; (most #'length '((1 2) (3 4 5 6) (7 8) (9)))
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setq wins obj
		    max score))))
	(values wins max))))

; (best #'> (list-gen 1 10))
(defun best (fn lst)
  (if (null lst)
      nil
      (let ((wins (car lst)))
	(dolist (obj (cdr lst))
	  (if (funcall fn obj wins)
	      (setq wins obj)))
	wins)))

; (mostn #'length '((1 2) (3 4 5 6) (7 8) (9) (7 5 6 8)))
(defun mostn (fn lst)
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
	    (max (funcall fn (car lst))))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (cond ((> score max)
		   (setq max    score
			 result (list obj)))
		  ((= score max)
		   (push obj result)))))
	(values (nreverse result) max))))

;;; Mapping functions

; (map0-n #'1+ 5)
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

; (mapa-b #'1+ -2 0 .5)
(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

; non-destructive alternative to mapcan
(defun mappend (fn &rest lsts)
  (apply #'append (apply #'mapcar fn lsts)))

; same as (mapcar #'fn (append lst1 lst2 ... lstn)), saves conses
(defun mapcars (fn &rest lsts)
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
	(push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
	     #'(lambda (&rest args)
		 (apply #'rmapcar fn args))
	     args)))

;;; I/O shit

(defun readlist (&rest args)
  (values (read-from-string
	   (concatenate 'string "("
			         (apply #'read-line args)
				 ")"))))

; (prompt "Enter a number between ~A and ~A.~%>> " 1 10)
(defun prompt (&rest args)
  (apply #'format *query-io* args)
  (read *query-io*))

; Finally, break-loop is for situations where you want to imitate the Lisp toplevel.
; It takes two functions and an &rest argument, which is repeatedly given to
; prompt. As long as the second function returns false for the input, the first
; function is applied to it. So for example we could simulate the actual Lisp
; toplevel with:
; > (break-loop #'eval #'(lambda (x) (eq x :q)) ">> ")
; Entering break-loop.
; >> (+ 2 3)
; 5
; >> :q
; :Q

(defun break-loop (fn quit &rest args)
  (format *query-io* "Entering break-loop.~%")
  (loop
   (let ((in (apply #'prompt args)))
     (if (funcall quit in)
	 (return)
	 (format *query-io* "~A~%" (funcall fn in))))))

; (mkstr pi " pieces of " 'pi)
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

; (symb 'ar "Madi" #\L #\L 0)
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

(defun reread (&rest args)
  (values (read-from-string (apply #'mkstr args))))

; (explode 'bomb)
(defun explode (sym)
  (map 'list #'(lambda (c)
		 (intern (make-string 1
				      :initial-element c)))
       (symbol-name sym)))

;;;; Functions that return functions

; return equivalent function fn with basic memoization
(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (&rest args)
	(multiple-value-bind (val win) (gethash args cache)
	  (if win
	      val
	      (setf (gethash args cache)
		    (apply fn args)))))))

; (funcall (compose #'1+ #'find-if) #'oddp '(2 3 4))
(defun compose (&rest fns)
  (if fns
      (let ((fn1 (car (last fns)))
	    (fns (butlast fns)))
	#'(lambda (&rest args)
	    (reduce #'funcall fns
		    :from-end t
		    :initial-value (apply fn1 args))))
      #'identity))


(defun fif (if then &optional else)
  #'(lambda (x)
      (if (funcall if x)
	  (funcall then x)
	  (if else (funcall else x)))))

;;; function intersection
(defun fint (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fint fns)))
	#'(lambda (x)
	    (and (funcall fn x) (funcall chain x))))))

;;; see above (uses (or) instead)
(defun fun (fn &rest fns)
  (if (null fns)
      fn
      (let ((chain (apply #'fun fns)))
	#'(lambda (x)
	    (or (funcall fn x) (funcall chain x))))))

;;;; Functions that recurse on CDRs

; (funcall (lrec #'(lambda (x f) (1+ (funcall f))) 0) '(1 2 3))
(defun lrec (rec &optional base)
  (labels ((self (lst)
	     (if (null lst)
		 (if (functionp base)
		     (funcall base)
		     base)
		 (funcall rec (car lst)
			  #'(lambda () (self (cdr lst)))))))
    #'self))
