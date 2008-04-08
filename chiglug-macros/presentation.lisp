;;;; Presentation Macro Demo

;;;; What are macros?
(defmacro sum (x y)
  `(+ ,x ,y))

;;;; syntax

(defun macro-01 (x)
  x)

(defun macro-02 (x)
  (x 20 30))

(defun macro-03 (x)
  '(x 20 30))

(defun macro-04 (x)
  (x '20 '30))

(defun macro-05 (x)
  `(x 20 30))

(defun macro-06 (x)
  `(,x 20 30))

;; how do we handled lists?

(defun macro-07 (lst)
  `(,lst 4 5))

(defun **---** (a)
  a)

(defun macro-08 (lst)
  `(,@lst 4 5))

;;;; differences between defun and defmacro
(defmacro sum (x y)
  `(+ ,x ,y))

(defun fsum (x y)
  `(+ ,x ,y))

;; lisp-1 vs. lisp-2
(define (sum x y)
    (+ x y))

(defun sum (x y)
  (+ x y))

;;;; when to use macros

;; necessity due to splicing in the body
(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun ex-while (n)
  (let ((i 0))
    (while (< i n)
      (princ i)
      (incf i))))

; (ex-while 10)
; (ex-while 0)

;; conditional evaluation, see while

;; picking apart the arguments (setf vs. setq (set-field vs. set-quantity))


;; compile-time compilation
(defun favg (&rest args)
  (/ (apply #'+ args) (length args)))

(defmacro mavg (&rest args)
  `(/ (+ ,@args) ,(length args)))

;; save function calls (just use (inline functionname))

;;;; common pitfalls -- behold (macroexpand-1)!

;; multiple evaluation
(defmacro square-01 (x)
  `(* ,x ,x))

;; fix it:
(defmacro square-02 (x)
  `(let ((val ,x))
     (* val val)))

;; hygiene!
;; what's wrong with this macro?
(defmacro for ((var start stop) &body body)
  `(do ((,var ,start (1+ ,var))
	(limit ,stop))
       ((> ,var limit))
     ,@body))

;; try and call it with a predifined symbol limit








(defmacro gen-for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

;; now try broken code

;;; omg cool examples!