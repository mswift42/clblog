(ql:quickload '(:hunchentoot :cl-who :elephant))

(defpackage #:clblog
  (:use :cl :hunchentoot :cl-who :elephant))

(in-package #:clblog)

;;; set the doctype string at the start of a HTML page to 'html5':
(setf (html-mode) :html5)

;; two macros to spare myself of repeating *standard-output* nil all
;; the time.
(defmacro with-html (&body body)
    `(with-html-output (*standard-output* nil)
       ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil)
     ,@body))

;; establish a db to store 
(defparameter *posts-db*
  (open-store '(:clsql (:sqlite3 "clblog.db"))))

(defmacro page-template ((&key title) &body body)
  "as all the blog pages use the same css files
   use a template to for the page-headers."
  `(with-html-string
       (:html
	(:head
	 (:title ,title))
	(:body ,@body))))












