
(in-package :cffi-epoll)

(defcfun ("epoll_create" c-epoll-create) :int
  (size :int))

(defun epoll-create (size)
  (let ((fd (c-epoll-create size)))
    (when (< fd 0)
      (error-errno "epoll-create"))
    fd))

(defcunion epoll-data
  (ptr :pointer)
  (fd :int)
  (u32 uint32-t)
  (u64 uint64-t))

(defcstruct epoll-event
  (events uint32-t)
  (data (:union epoll-data)))

(defcfun ("epoll_ctl" c-epoll-ctl) :int
  (epfd :int)
  (op :int)
  (fd :int)
  (event (:pointer (:struct epoll-event))))

(defun epoll-ctl (epfd op fd events data)
  (with-foreign-object (evt '(:struct epoll-event))
    (setf (foreign-slot-value evt '(:struct epoll-event) 'events) events
	  (foreign-slot-value evt '(:struct epoll-event) 'data) data)
    (let ((r (c-epoll-ctl epfd op fd evt)))
      (when (< r 0)
	(error-errno "epoll_ctl"))
      r)))

(defcfun ("epoll_wait" c-epoll-wait) :int
  (epfd :int)
  (events (:pointer (:struct epoll-event)))
  (maxevents :int)
  (timeout :int))

(defun epoll-wait (epfd &optional (maxevents 64) (timeout -1))
  (with-foreign-object (events '(:struct epoll-event) maxevents)
    (c-epoll-wait )))
