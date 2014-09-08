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

(defmethod (setf title) (nt (p persistent-post))
  (setf (slot-value p 'title) nt))

(defmethod (setf body) (nb (p persistent-post))
  (setf (slot-value p 'body) nb))



(defun blogposts ()
  "return the stored blogposts"
  (nreverse (get-instances-by-range 'persistent-post 'id nil nil)))

(defun single-blog-entry (id)
  "return the stored blogpost belonging to Id id"
  (get-instance-by-value 'persistent-post 'id id))


(defmacro blog-post-form (target title content id &rest delete)
  "generate html for the form used in saving and editing a form."
  `(with-html
    (:form :method :post
           :action ,target
           (:div :class "form-group" "Blog Title"
                 (:input :type "text" :name "title"
                         :class "form-control" :label "Title"
                         :value ,title))
           (:div :class "form-group" "Content"
                 (:textarea :name "body" :class "form-control"
                            :label "Content" :rows "20"
                            :value ,content))
           (:div :class "form-group"
                 (:input :type "hidden" :name "id" :value ,id)
                 (:input :type "submit" :value "Submit" :name "blogidbutton"
                         :class "btn btn-default")
                 ,@delete))))

(defun newpost-page ()
  (with-html
    (page-template (:title "New Posts")
      (navbar-header "newpost")
      (:div :class "container"
            (:h3 :class "header" "New Posts")
            (blog-post-form "/addpost" "" "" "" )))))

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



(defvar *blog-posts*
  (or (get-from-root "blog-posts")
      (let ((blog-posts (make-pset)))
        (add-to-root "blog-posts" blog-posts)
        blog-posts)))

(define-easy-handler (newpost :uri "/newpost")
    ()
  (newpost-page))


(defun display-blog-entries (entries)
  "html to display for all blog entries."
  (with-html-string
    (:div :class "entries"
          (:div :class "entry"
                (loop for i in entries do
                     (htm
                      (:div :class "singleentry"
                            (:div :class "posttitle"
                                  (:p (str (title i))))
                            (:div :class "postbody"
                                  (:p (str (body i))))
                            (:div :class "postid"
                                  (:form :method "post" :action "/editpost"
                                         (:input :type "hidden" :value (str (id i)) :name "entryid")
                                         (:input :type "submit" :class "btn btn-default" :value "Edit Blog Entry"))))))))))

(defvar *web-server* (make-instance 'easy-acceptor :port 4242))




(defun index-page (posts)
  (with-html
    (page-template (:title "Index")
      (navbar-header "index")
      (:div :class "container"
            (:h3 :class "header" "Blog Posts")
            (str (display-blog-entries posts))))))

(define-easy-handler (index :uri "/index")
    ()
   (index-page (blogposts)))

(define-easy-handler (addpost :uri "/addpost")
    ((title) (body))
  (add-blog-post title body)
  (redirect "/index"))

(define-easy-handler (editpost :uri "/editpost")
    (entryid)
  (edit-blog-post entryid))

(defun edit-blog-post (id)
  "lookup blog-post entry with 'id' id and display form html with its
   contents."
  (let ((post (single-blog-entry (parse-integer id))))
    (with-html
      (page-template (:title "Edit Posts")
        (navbar-header "")
        (:div :class "container"
              (:h3 :class "header" "Edit Posts")
              (blog-post-form "/saveedit" (title post) (body post) id
                              (htm (:button :class "btn btn-danger" :name "blogidbutton" :value "Delete" "Delete"))))))))

(define-easy-handler (saveedit :uri "/saveedit")
    ((title) (body)  (id))
  (if (string-equal "Submit" (post-parameter "blogidbutton"))
      (edit-post id title body)
      (delete-post id))
  (redirect "/index"))

(defun edit-post (id title body)
  (let ((post (single-blog-entry (parse-integer id))))
    (setf (title post) title)
    (setf (body post) body)))

(defun delete-post (id)
  "Delete blogpost with 'id' id"
  (let ((post (single-blog-entry (parse-integer id))))
    (with-transaction ()
      (remove-from-root post))))

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















