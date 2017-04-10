
(in-package :common-lisp)

(defpackage :cffi-epoll
  (:use :common-lisp :cffi :cffi-errno)
  (:export
   #:create
   #:ctl
   #:wait))
