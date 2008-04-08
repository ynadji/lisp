(in-package :cl-user)

(require 'hunchentoot)
(require 'cl-who)

(defvar *server* (hunchentoot:start-server :port 8080))

(push (hunchentoot:create-regex-dispatcher "^/$" 'reddit-home)
      hunchentoot:*dispatch-table*)

(defclass link ()
  ((title
    :reader title
    :initarg :title)
   (url
    :reader url
    :initarg :url)))

(defvar *links* nil)

(defun reddit-home ()
  (cl-who:with-html-output-to-string (str)
    (:html
     (:head (:title "Reddit in Lisp!"))
     (:body
      (:h1 "A Reddit implementation in Common Lisp")
      (:h2 "Using sbcl and hunchentoot")
      (:h3 "Links")
      (:ol
       (dolist (l *links*)
	 (cl-who:htm (:li (:a :href (url l)
			      (cl-who:str (title l))))))))
     )))