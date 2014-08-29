(ql:quickload '(:hunchentoot :cl-who :elephant ))

(defpackage #:clblog
  (:use :cl :hunchentoot :cl-who :elephant ))

(in-package #:clblog)

;;; set the doctype string at the start of a HTML page to 'html5':
(setf (html-mode) :html5)

;; two macros to spare myself of repeating *standard-output* nil all
;; the time.
(defmacro with-html (&body body)
    `(with-html-output (*standard-output* nil :indent t)
       ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
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
	 (:link :type "text/css" :rel "stylesheet"
                :href "//netdna.bootstrapcdn.com/bootstrap/3.2.0/css/bootstrap.min.css")
	 (:link :type "text/css" :rel "stylesheet" :href "/blog.css"))
	(:body ,@body))))

(defun navbar-header (active)
  "return bootstrap navbar with active element marked 'active'."
  (with-html
    (:nav :class "navbar navbar-fixed-top" :role "navigation"
          (:div :class "container-fluid"
                (:div :class "navbar-header"
                      (:a :class "navbar-brand" :href "#" "Clblog"))
                (:div :class "navbar"
                      (:ul :class "nav navbar-nav"
                           (dolist (i '(("Home" "/index") ("New Blog Entry" "/newpost")
                                        ("About" "/about")))
                             (if (search active (second i))
                                 (htm (:li :class "active"
                                           (:a :href (second i) (str (first i)))))
                                 (htm (:li (:a :href (second i) (str (first i)))))))))))))

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
  (with-html
    (page-template (:title "New Posts")
      (navbar-header "newpost")
      (:div :class "container"
            (:h3 :class "header" "New Posts")
            (:div :class "form-group"
                  (:form :method :post 
                         :action "/addpost"
                         (:div :class "form-group" "Title"
                               (:input :type "text" :name "title"
                                       :class "form-control" :label "Title"))
                         (:div :class "form-group" "Blog Content"
                               (:textarea :name "body" :class "form-control"
                                          :label "Content" :rows "20"))
                         (:div :class "form-group"
                               (:input :type "submit"   :value "Submit"
                                       :class "btn btn-default"))))))))

(defun add-blog-post (title body)
  "add a new blogpost to db with title and body,
   but only when title AND body are non empty."
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



(defun display-blog-entries (entries)
  "html to display for all blog entries."
  (with-html-string
    (:div :class "entires"
          (:div :class "entry"
                (loop for i in entries do
                     (htm
                      (:div :class "singleentry"
                            (:div :class "posttitle"
                                  (:p (str (title i))))
                            (:div :class "postbody"
                                  (:p (str (body i))))
                            (:div :class "postid"
                                  (:p (str (id i)))))))))))

(defvar *web-server* (make-instance 'easy-acceptor :port 4242))




(defun index-page (posts)
  (with-html
    (page-template (:title "Index")
      (navbar-header "index")
      (:div :class "container"
            (:h3 :class "header" "Watch your posts")
            (str (display-blog-entries posts))))))

(define-easy-handler (index :uri "/index")
    ()
   (index-page (blogposts)))

(define-easy-handler (addpost :uri "/addpost")
    ((title) (body))
  (add-blog-post title body)
  (redirect "/index"))


(defparameter *about*
  (with-html
    (page-template (:title "About")
      (navbar-header "about")
      (:div :class "container"
           (:h3 :class "header" "About")
           (:div :class "jumbotron"
                 (:p "A simple implementation of a blog using Common Lisp, ")
                 (:p "Hunchentoot, elephant and cl-who."))))))

(define-easy-handler (about :uri "/about")
    ()
    *about*)















