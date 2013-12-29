(ql:quickload '(:hunchentoot :cl-who :elephant :parenscript))

(defpackage #:clblog
  (:use :cl :hunchentoot :cl-who :elephant :parenscript))

(in-package #:clblog)

;;; set the doctype string at the start of a HTML page to 'html5':
(setf (html-mode) :html5)

;; two macros to spare myself of repeating *standard-output* nil all
;; the time.
(defmacro with-html (&body body)
    `(with-html-output (*standard-output* nil :prologue t :indent t)
       ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     ,@body))


;; tell Hunchentoot which css and js file to use.
(push (create-static-file-dispatcher-and-handler
       "/blog.css" "blog.css") *dispatch-table*)
(push (create-static-file-dispatcher-and-handler
       "/jquery.js" "jquery-2.0.3.min.js") *dispatch-table*)


(defmacro page-template ((&key title) &body body)
  "as all the blog pages use the same css files
   use a template to for the page-headers."
  `(with-html-string
       (:html
	(:head
	 (:title ,title)
	 (:link :type "text/css" :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css")
	 (:link :type "text/css" :rel "stylesheet" :href "/blog.css")
	 (:script :src "/jquery.js"))
	(:body ,@body))))

(defvar *id-counter* 0)

(defun get-id ()
  "return a single integer for the current time
   in universal time format."
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

(defun newpost-page ()
    (page-template (:title "New Posts")
      (htm
	(:h3 :class "header" "Fill in Title and your blogpost to submit a new post")
	(:div :class "form-group"
	      (:form :method :post :onsubmit (ps-inline
					      (when (or
						     (= title.value "")
						     (= body.value ""))
						(alert "you need body and title")))
		   ;  :action "/index"
		     (:div :class "form-group"
			   (:input :type "text" :name "title"
				   :class "form-control" :label "Title"))
		     (:div :class "form-group"
			   (:textarea :name "body" :class "form-control"
				      :label "Content" :rows "20"))
		     (:div :class "form-group"
			   (:input :type "submit"   :value "Submit"
				   :class "btn btn-default")))))))

(defun add-blog-post (title body)
  "add a new blogpost to db with title and body"
  (let ((trimmed-title (string-trim " " title))
	(trimemed-body (string-trim " " body)))
    (with-transaction ()
	(make-instance 'persistent-post :title trimmed-title
		       :body trimemed-body))))

(define-easy-handler (newpost :uri "/newpost")
    ()
  (newpost-page))

(defun display-bloglist (list)
  "iterate through list of stored blogpost instances and 
   display them."
  (with-html-string
    (dolist (i list)
      (htm
       (:p (str (title i)))
       (:p (str (body i)))
       (:p (str (id i)))))))

(defvar *web-server* (make-instance 'easy-acceptor :port 4242))

(defparameter *index*
  (with-html
    (page-template (:title "Index")
      (:h3 :class "header" "Watch your posts")
      (display-bloglist (blogposts)))))

(define-easy-handler (index :uri "/index")
    ()
  *index*)


















