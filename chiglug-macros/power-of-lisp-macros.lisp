;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10; Encoding: latin-1; -*-
;;; $Header: /usr/local/cvsrep/weitz.de/macros.lisp,v 1.17 2006/12/31 11:25:36 edi Exp $

(in-package :cl-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                       ;;;
;;;       THE POWER OF LISP MACROS        ;;;
;;;                                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;; 1. What makes Lisp different
;;; ----------------------------
;;;
;;; <http://www.paulgraham.com/diff.html>
;;;
;;; - A notation for code as trees of symbols
;;; - The whole language always available (read, compile, run)
;;;
;;; Aims of this talk:
;;;
;;; - Give 30 minute intro to macros for Lisp "newbies"
;;;   (while sweeping several details under the rug)
;;; - Show what's cool about them
;;;   (the macros, not the newbies...)
;;; - Wetten your appetite for more (see URLs)

;;; See also:
;;;
;;; - Casting SPELs in Lisp
;;;   <http://www.lisperati.com/casting.html>
;;; - Practical Common Lisp
;;;   <http://www.gigamonkeys.com/book/>


;;; 2. A function is not enough
;;; ---------------------------
;;;
;;; Adding new control constructs to the language

(defun red-button ()
  (capi:display-message "BOOM! Game Over..."))

(if (= 1 1)
  "No worries..."
  (red-button))

(defun my-if (test-form then-form else-form)
  (cond (test-form then-form)
        (t else-form)))

(my-if (= 1 1)
  "No worries..."
  (red-button))

;;; Oops...

(defmacro my-if (test-form then-form else-form)
  ;; in "reality" IF is a "special operator" and COND is a macro
  (list 'cond
        (list test-form then-form)
        (list t else-form)))

;;; Now try to MACROEXPAND-1 this:

(my-if (= 1 1)
  "No worries..."
  (red-button))

;;; That's better... :)

(defmacro my-if (test-form then-form else-form)
  `(cond (,test-form ,then-form)
         (t ,else-form)))

;;; More sophisticated examples:
;;;
;;; - The LOOP macro
;;;   (see for example SBCL source code at <http://sbcl.org/>)
;;; - Baker: Metacircular Semantics for CL Special Forms
;;;   <http://home.pipeline.com/~hbaker1/MetaCircular.html>



;;; 3. The classic: WITH-
;;; ---------------------
;;;
;;; Wrap a body of code with a prologue
;;; and a (guaranteed) happy ending

(defun begin-transaction ()
  (format t "Starting transaction~%"))

(defun commit-transaction ()
  (format t "Committing transaction~%"))

(defun abort-transaction ()
  (format t "Aborting transaction~%"))

(defmacro with-transaction (&body body)
  `(let (done) ; variable capture - see below
     (unwind-protect
         (prog2
             (begin-transaction)
             (progn ,@body)
           (setq done t))
       (if done
         (commit-transaction)
         (abort-transaction)))))

(defvar *in-transaction-p* nil)

(defmacro with-transaction (&body body)
  `(let (done) ; variable capture - see below
     (flet ((body-fn () ,@body))
       (cond (*in-transaction-p* (body-fn))
             (t (unwind-protect
                    (let ((*in-transaction-p* t))
                      (prog2
                          (begin-transaction)
                          (body-fn)
                        (setq done t)))
                  (if done
                    (commit-transaction)
                    (abort-transaction))))))))

;;; See also:
;;;
;;; - Typical UNWIND-PROTECT macros:
;;;   - WITH-OPEN-FILE (ANSI standard)
;;;     <http://www.lispworks.com/documentation/HyperSpec/Body/m_w_open.htm>
;;;   - WITH-LOCK/WITH-MUTEX (usually in Lisps with MP support)
;;;     <http://www.sbcl.org/manual/Mutex-Support.html>
;;;   - WITH-DYNAMIC-FOREIGN-OBJECTS (LispWorks FLI)
;;;     <http://www.lispworks.com/documentation/lw50/FLI/html/fli-125.htm>
;;;   - WITH-TIMEOUT (e.g. in ACL-COMPAT)
;;;     <http://www.cl-user.net/asp/libs/acl-compat>
;;; - Other WITH- macros:
;;;   - WITH-ACCESSORS (ANSI standard)
;;;     <http://www.lispworks.com/documentation/HyperSpec/Body/m_w_acce.htm>
;;;   - WITH-STANDARD-IO-SYNTAX (ANSI standard)
;;;     <http://www.lispworks.com/documentation/HyperSpec/Body/m_w_std_.htm>
;;;   - WITH-OUTPUT-TO-STRING (ANSI standard)
;;;     <http://www.lispworks.com/documentation/HyperSpec/Body/m_w_out_.htm>
;;;   - WITH-HTML-OUTPUT (CL-WHO)
;;;     <http://weitz.de/cl-who/#with-html-output>
;;; - lots more...



;;; 4. Ensuring discretion
;;; ----------------------
;;;
;;; Avoiding variable capture

(let ((done 42))
  (with-transaction
    (incf done))
  done)

;;; Ouch...

(defmacro with-transaction (&body body)
  (let ((done (gensym)))
    `(let (,done)
       (flet ((body-fn () ,@body))
         (cond (*in-transaction-p* (body-fn))
               (t (unwind-protect
                      (let ((*in-transaction-p* t))
                        (prog2
                            (begin-transaction)
                            (body-fn)
                          (setq ,done t)))
                    (if ,done
                      (commit-transaction)
                      (abort-transaction)))))))))

;;; But we can do that with a macro as well... :)

(defmacro with-unique-names* (vars &body body)
  ;; LispWorks already has WITH-UNIQUE-NAMES...
  `(let ,(loop for var in vars
               collect `(,var (gensym ,(symbol-name var))))
     ,@body))

(defmacro with-transaction (&body body)
  (with-unique-names* (done)
    `(let (,done)
       (flet ((body-fn () ,@body))
         (cond (*in-transaction-p* (body-fn))
               (t (unwind-protect
                      (let ((*in-transaction-p* t))
                        (prog2
                            (begin-transaction)
                            (body-fn)
                          (setq ,done t)))
                    (if ,done
                      (commit-transaction)
                      (abort-transaction)))))))))

;;; See also:
;;;
;;; - REBINDING
;;;   e.g. at <http://www.cliki.net/Common%20Lisp%20Utilities>
;;; - various LET-like constructs
;;;   e.g. <http://www.geocities.com/mparker762/with.html>
;;;   or <http://common-lisp.net/project/cl-containers/metabang-bind/>



;;; 5. To macro or not to macro
;;; ---------------------------
;;;
;;; Don't use macros when functions will do

(defun one+ (x)
  (1+ x))

(mapcar 'one+ '(1 2 3))
(funcall #'one+ 41)

;;; Look, ma, no function call overhead:

(defmacro one+ (x)
  `(1+ ,x))

(mapcar 'one+ '(1 2 3))
(funcall #'one+ 41)

;;; Cough...

;;; Alternatives:
;;;
;;; - INLINE (see ANSI standard)
;;;   <http://www.lispworks.com/documentation/HyperSpec/Body/d_inline.htm>
;;; - Compiler macros (see below)



;;; 6. A (well, very simple) unit test framework in 21 seconds
;;; ----------------------------------------------------------
;;;
;;; Execute compiled code at run time and keep the source as well

(defvar *test-thunks* (make-hash-table)) ; <http://en.wikipedia.org/wiki/Thunk>
(defvar *test-sources* (make-hash-table))

(defmacro define-test (name (&optional condition) &body body)
  (with-unique-names (c)
    `(setf (gethash ',name *test-sources*) '(progn ,@body)
           (gethash ',name *test-thunks*)
           (lambda ()
             (handler-case
                 (format t "Test ~A ~:[FAILED~;passed~].~%"
                         ',name (progn ,@body))
               ,@(when condition
                   `((,condition () (format t "Test ~A passed.~%"
                                            ',name))))
               (error (,c)
                 (format t "Test ~A FAILED, ~
                              condition ~S was signalled.~%"
                         ',name ,c)))))))

(defun run-tests ()
  (dolist (test-name (sort (loop for name being the hash-keys of *test-thunks*
                                 collect name)
                           #'string-lessp))
    (format t "~%~%Starting test ~A...~%" test-name)
    (let ((*print-pretty* t))
      (format t "~S~%~%" (gethash test-name *test-sources*)))
    (funcall (gethash test-name *test-thunks*)))
  (values))

(define-test simple-plus-test ()
  (= (+ 1 1) 2))

(define-test wake-up-after-sleep ()
  (sleep 1) t)

(define-test division-by-zero (division-by-zero)
  (let ((a 42) (b 41))
    (incf b)
    (/ 1 (- a b))))

(define-test unknown-file ()
  ;; this is a Windows laptop...
  (open "/etc/passwd"))

(run-tests)

;;; See also:
;;; 
;;; - "Real" test frameworks
;;;   <http://www.cl-user.net/asp/tags/unit-testing>
;;; - Some other things that look like DEFUN:
;;;   - DEF-FUNCTION (UFFI)
;;;     <http://uffi.b9.com/manual/def-function.html>
;;;   - DEFINE-EASY-HANDLER (Hunchentoot)
;;;     <http://weitz.de/hunchentoot/#define-easy-handler>
;;;   - DEFINE-PLUGIN-FUNCTION (FM-PLUGIN-TOOLS)
;;;     <http://weitz.de/fm-plugin-tools/#define-plugin-function>
;;;   - lots more...



;;; 7. Your HTML tutor
;;; ------------------
;;;
;;; Macros writing macros

(defmacro emit-html ((tag &rest attributes) &body body)
  `(progn
     (format t "<~(~A~)" ',tag)
     ,@(loop for (key value) on attributes by #'cddr
             when value
             collect (rebinding (value)
                       `(format t " ~(~A~)~:[=\"~A\"~;~]"
                                ',key (eq ,value t) ,value)))
     (format t ">")
     ,@(loop for expr in body
             when (stringp expr) collect `(format t ,expr)
             else collect expr)
     ,@(when body `((format t "</~(~A~)>" ',tag)))
     (terpri t)))

(emit-html (hr :align "left" :noshade t))
(emit-html (p :align "left")
  "Part one"
  (emit-html (br))
  "Part two"
  (emit-html (hr :size (+ 39 3) :noshade nil))
  "The end")

(defmacro define-html-tag (tag &rest attr-list)
  (let* ((no-body (find :no-body attr-list
                        :test #'string-equal))
         (attr-list (remove :no-body attr-list
                            :test #'string-equal))
         (attr-args (loop for attr-name in attr-list
                          collect (list 'quote attr-name)
                          collect attr-name)))
    (with-unique-names (body)
      `(defmacro ,tag ((&key ,@attr-list)
                       ,@(unless no-body `(&body ,body)))
         `(emit-html (,',tag ,,@attr-args)
            ,@,(unless no-body body))))))

(define-html-tag hr no-body align noshade size)
(define-html-tag br no-body clear)
(define-html-tag p align)

;;; note argument list display in IDE:

(p (:align "left")
  "Part one"
  (br ())
  "Part two"
  (hr (:size (+ 40 2) :noshade t))
  "The end")

;;; Other HTML macros
;;; at <http://www.cl-user.net/asp/tags/html-macros>


;;; 8. For your speed only
;;; ----------------------
;;;
;;; Compiler macros - transform code to help the compiler

(defun add-vat (amount &key (tax-rate .19))
  (* (+ 1 tax-rate) amount))

(defun add-vat% (amount tax-rate)
  (format t "Function with simple lambda list called...~%")
  (* (+ 1 tax-rate) amount))

(define-compiler-macro add-vat (amount &key (tax-rate .19))
  `(add-vat% ,amount ,tax-rate))

(defun vat-test ()
  (format t "Test 1~%")
  (format t "=> ~A~%"
          (add-vat 30000 :tax-rate .16))
  (format t "~%Test 2~%")
  (let ((more-args (list :tax-rate .16)))
    (format t "=> ~A~%"
            (apply #'add-vat 30000 more-args))))

;;; see <http://weitz.de/cl-ppcre/>
(asdf:oos 'asdf:load-op :cl-ppcre)

(trace ppcre::create-scanner)

(ppcre:scan "^\\s*[a-z]+\\s*$" " abc ")

(defun several-scans (&rest strings)
  (loop for string in strings
        count (ppcre:scan "^\\s*[a-z]+\\s*$" string)))

;;; See also:
;;; 
;;; - Talk by Arthur Lemmens with several examples
;;;   <http://www.pentaside.org/paper/compilermacro-lemmens/compiler-macros-for-publication.txt>



;;; 9. What you see is not always what you get
;;; ------------------------------------------
;;;
;;; Reader macros

"The present-day composer refuses to die.\n\n\t\t\tEdgar Varèse"

(defun new-double-quote-reader (stream macro-char)
  (declare (ignore macro-char))
  (with-output-to-string (out)
    (loop for char = (read-char stream t nil t)
          while (char/= char #\")
          do (write-char
              (cond ((char= char #\\)
                     (let ((next-char (read-char stream t nil t)))
                       (case next-char
                         (#\t #\Tab)
                         (#\n #\Newline)
                         (otherwise next-char))))
                    (t char))
              out))))

(set-macro-character #\" 'new-double-quote-reader)

;;; See also:
;;; 
;;; - Symbolic SQL syntax (LispWorks Common SQL / CLSQL)
;;;   <http://www.lispworks.com/documentation/sql-tutorial/index.html>
;;;   <http://www.lispworks.com/documentation/lw445/LWUG/html/lwuser-243.htm>
;;;   <http://clsql.b9.com/>
;;; - Embedded XML (XMLisp)
;;;   <http://www.agentsheets.com/lisp/XMLisp/>
;;; - Embedded .NET calls (RDNZL)
;;;   <http://weitz.de/rdnzl/#reader>
;;; - Shell-like variable interpolations (CL-INTERPOL)
;;;   <http://weitz.de/cl-interpol/>
;;; - Internationalization



;;; 10. Don't try this at home
;;; --------------------------
;;;
;;; Macros for black belts

;;; - "On Lisp" <http://www.paulgraham.com/onlisp.html>
;;; - Screamer <http://www.cl-user.net/asp/libs/screamer>
;;; - Series <http://series.sourceforge.net/>
;;; - Iterate <http://common-lisp.net/project/iterate/>



;;; The latest version of this document is available from
;;; <http://weitz.de/macros.lisp>.

;;; Note that the code snippets above are quick hacks for demo
;;; purposes; you should /not/ assume they're error-free or even
;;; production-quality.

;;; This talk was originally given at the company Christmas party 2006
;;; of freiheit.com technologies gmbh.

;;; Thanks to Arthur Lemmens for various suggestions for this talk.


;;; Copyright (c) 2006, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use of this text in its orginal form (plain
;;; text) or in 'derived' forms (HTML, PDF, Postscript, RTF and so
;;; forth) with or without modification, are permitted provided that
;;; the following condition is met:

;;; * Redistributions must reproduce the above copyright notice, this
;;;   condition and the following disclaimer in the document itself
;;;   and/or other materials provided with the distribution.

;;; THIS DOCUMENT IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; DOCUMENTATION, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
