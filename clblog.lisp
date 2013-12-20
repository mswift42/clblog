(ql:quickload '(:hunchentoot :cl-who))

(defpackage #:clblog
  (:use :cl :hunchentoot :cl-who))

(in-package #:clblog)

;; two macros to spare myself of repeati ng *standard-output* nil all
;; the time.
(defmacro with-html (&body body)
    `(with-html-output (*standard-output* nil)
       ,@body))

(defmacro with-html-string (&body body)
  `(with-html-output-to-string (*standard-output* nil)
     ,@body))




