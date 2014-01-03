(ql:quickload '(:hunchentoot :cl-who :elephant :parenscript))

(defpackage #:clblog
  (:use :cl :hunchentoot :cl-who :elephant :parenscript))

(in-package #:clblog)

;;; set the doctype string at the start of a HTML page to 'html5':
(setf (html-mode) :html5)
(setf hunchentoot:*show-lisp-errors-p* t)
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
	 (:link :type "text/css" :rel "stylesheet"
                :href "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css")
	 (:link :type "text/css" :rel "stylesheet" :href "/blog.css")
	 (:script :src "/jquery.js"))
	(:body ,@body))))


(defun get-id ()
  "return a single integer for the current time
   in universal time format."
  (get-universal-time))


(defvar *store* (open-store '(:clsql (:sqlite3 "blog.db"))))

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
       	(:nav :class "navbar navbar-inverse" :role "navigation"
	      (:div :class "navbar-header"
		    (:a :class "navbar-brand" :href "/index" "Index")
		    (:a :class "navbar-brand" :href "/about" "About")))
	(:h3 :class "header" "New Posts")
	(:div :class "form-group"
	      (:form :method :post :onsubmit (ps:ps-inline
					      (if (or
						     (= title.value "")
						     (= body.value ""))
                                                  (alert "you need body and title")))
                     :action "/addpost"
		     (:div :class "form-group" "Title"
			   (:input :type "text" :name "title"
				   :class "form-control" :label "Title"))
		     (:div :class "form-group" "Blog Content"
			   (:textarea :name "body" :class "form-control"
				      :label "Content" :rows "20"))
		     (:div :class "form-group"
			   (:input :type "submit"   :value "Submit"
				   :class "btn btn-default")))))))

(defun add-blog-post (title body)
  "add a new blogpost to db with title and body"
  (let ((trimmed-title (string-trim " " title))
	(trimmed-body (string-trim " " body)))
    (unless (or (string-equal "" trimmed-title)
                (string-equal "" trimmed-body))
      (with-transaction ()
	(make-instance 'persistent-post :title trimmed-title
		       :body trimmed-body)))))

(define-easy-handler (newpost :uri "/newpost")
    ()
  (newpost-page))

(defun display-bloglist (list)
  "iterate through list of stored blogpost instances and 
   display them."
  (with-html-string
    (:thead
     (:tr
      (:th "#")
      (:th "Title")
      (:th "Blog Content")
      (:th "id")))
    (:tbody
     (loop for i in list and count from 1 do
       (htm
        (:tr
         (:th (str count))
         (:th (str (title i)))
         (:th (str (body i)))
         (:th (str (id i)))))))))

(defvar *web-server* (make-instance 'easy-acceptor :port 4242))

(defun navbar (links)
  "return a bootstrap navbar for all links in 'links'"
  (with-html
    (:nav :class "navbar navbar-inverse" :role "navigation"
          (:div :class "navbar-header"
                (dolist (i links)
                  (htm
                   (:a :class "navbar-brand" :href (first i)
                       (str (second i)))))))))

(defun index-page (posts)
  (with-html
    (page-template (:title "Index")
      (navbar '(("/newpost" "Write a new post")
                ("/about" "About")))
      (:h3 :class "header" "Watch your posts")
      (:table :class "table table-striped"
              (str (display-bloglist posts))))))

(define-easy-handler (index :uri "/index")
    ()
   ;; only save blog-post if title
  (let ((posts (blogposts)))
    (index-page posts))) ;; and body are non-nil

(define-easy-handler (addpost :uri "/addpost")
    ((title) (body))
  (add-blog-post title body)
  (redirect "/index"))


(defparameter *about*
  (with-html
    (page-template (:title "About")
      (navbar '(("/newpost" "Write a new post")
                ("/index" "Read up on the latest posts.")))
      (:h3 :class "header" "About")
      (:div :class "about"
            (:p "A simple implementation of a blog using Common Lisp, ")
            (:p "Hunchentoot, parenscript, elephant and cl-who.")))))

(define-easy-handler (about :uri "/about")
    ()
    *about*)















