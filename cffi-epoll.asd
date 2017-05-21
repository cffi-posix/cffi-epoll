
(in-package :common-lisp-user)

(defpackage :cffi-epoll.system
  (:use :common-lisp :asdf))

(in-package :cffi-epoll.system)

(defsystem "cffi-epoll"
  :defsystem-depends-on ("cffi-grovel")
  :depends-on ("cffi" "cffi-errno" "cffi-unistd")
  :components
  ((:file "package")
   (:cffi-grovel-file "grovel-epoll" :depends-on ("package"))
   (:file "cffi-epoll" :depends-on ("grovel-epoll"))))
