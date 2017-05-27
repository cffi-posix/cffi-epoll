
(in-package :common-lisp-user)

(defpackage :cffi-epoll.system
  (:use :common-lisp :asdf))

(in-package :cffi-epoll.system)

(defsystem :cffi-epoll
  :name "cffi-epoll"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Common Lisp wrapper for Linux epoll syscall"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-errno" "cffi-unistd")
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel-epoll" :depends-on ("package"))
   (:file "cffi-epoll" :depends-on ("grovel-epoll"))))
