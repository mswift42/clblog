(ql:quickload '(:hunchentoot :cl-who :elephant :parenscript))

(defpackage #:clblog
  (:use :cl :hunchentoot :cl-who :elephant :parenscript))

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


;; tell Hunchentoot which css file to use.
(push (create-static-file-dispatcher-and-handler
       "/blog.css" "blog.css") *dispatch-table*)


(defmacro page-template ((&key title) &body body)
  "as all the blog pages use the same css files
   use a template to for the page-headers."
  `(with-html-string
       (:html
	(:head
	 (:title ,title)
	 (:link :type "text/css" :rel "stylesheet" :href "/blog.css")
	 (:script :src "http://code.jquery.com/jquery-latest.min.js"))
	(:body ,@body))))

(defvar *id-counter* 0)

(defun get-id ()
  "return a single integer for the current time
   in a universal time format."
  (get-universal-time))


(defparameter *store* (open-store '(:clsql (:sqlite3 "blog.db"))))


(defpclass persistent-post ()
  ((title :reader title :initarg :title :index t)
   (body :reader body :initarg :body :index t)
   (id :reader id :initarg :id :initform (get-id) :index t))
  (:documentation "each blogpost with id, title and body
        is an instance of the Elephant persistent class"))

(defun blogposts ()
  "return the stored blogposts"
  (nreverse (get-instances-by-range 'persistent-post 'id nil nil)))

(defun newpost-page (title body)
  "html for /newpost page"
  (page-template (:title "New Posts")
    (with-html
      (:h3 :class "header" "Fill in Title and your blogpost to submit a new post")
      (:div :class "forms"
	    (:form :method :post :action "/index"
		   (:div :class "titletext"
			 (:input :type "text" :name title))
		   (:div :class "bodytext"
			 (:textarea :name body))
		   (:div :class "submitbutton"
			 (:input :type "submit"   :value "Submit")))))))

(define-easy-handler (newpost :uri "/newpost")
    ((title) (body))
  (newpost-page "" ""))

(defvar *web-server* (make-instance 'easy-acceptor :port 4242))

















