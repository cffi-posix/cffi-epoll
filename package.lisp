
(in-package :common-lisp)

(defpackage :cffi-epoll
  (:nicknames :epoll)
  (:use
   :cffi
   :common-lisp
   :errno)
  (:shadow
   #:mod)
  (:export
   #:create
   #:with
   #:ctl
   #:add
   #:mod
   #:del
   #:wait
   #:+ctl-add+
   #:+ctl-mod+
   #:+ctl-del+
   #:+in+
   #:+out+
   #:+rdhup+
   #:+pri+
   #:+err+
   #:+hup+
   #:+et+
   #:+exclusive+))
