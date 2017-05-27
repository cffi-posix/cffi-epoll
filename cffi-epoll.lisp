
(in-package :cffi-epoll)

(defcfun ("epoll_create" c-epoll-create) :int
  (size :int))

(defun create (&optional (size 10))
  (let ((fd (c-epoll-create size)))
    (when (< fd 0)
      (error-errno "epoll_create"))
    fd))

(defmacro with ((fdvar &optional (size 10)) &body body)
  (let ((fd (gensym "FD-")))
    `(let ((,fd (create ,size)))
       (unwind-protect (let ((,fdvar ,fd)) ,@body)
	 (unistd:close ,fd)))))

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

(defun ctl (epfd op fd events &key data-ptr data-fd data-u32 data-u64)
  (with-foreign-object (evt '(:struct epoll-event))
    (setf (foreign-slot-value evt '(:struct epoll-event) 'events) events)
    (let ((data (foreign-slot-value evt '(:struct epoll-event) 'data)))
      (with-foreign-slots ((ptr fd u32 u64) data (:union epoll-data))
	(cond (data-ptr (setf ptr data-ptr))
	      (data-fd  (setf fd  data-fd))
	      (data-u32 (setf u32 data-u32))
	      (data-u64 (setf u64 data-u64)))))
    (let ((r (c-epoll-ctl epfd op fd evt)))
      (when (< r 0)
	(error-errno "epoll_ctl"))
      r)))

(defun add (epfd fd events &key data-ptr data-fd data-u32 data-u64)
  (ctl epfd +ctl-add+ fd events
       :data-ptr data-ptr
       :data-fd  data-fd
       :data-u32 data-u32
       :data-u64 data-u64))

(defun mod (epfd fd events &key data-ptr data-fd data-u32 data-u64)
  (ctl epfd +ctl-mod+ fd events
       :data-ptr data-ptr
       :data-fd  data-fd
       :data-u32 data-u32
       :data-u64 data-u64))

(defun del (epfd fd events &key data-ptr data-fd data-u32 data-u64)
  (ctl epfd +ctl-del+ fd events
       :data-ptr data-ptr
       :data-fd  data-fd
       :data-u32 data-u32
       :data-u64 data-u64))

(defcfun ("epoll_wait" c-epoll-wait) :int
  (epfd :int)
  (events (:pointer (:struct epoll-event)))
  (maxevents :int)
  (timeout :int))

(defmacro wait ((events-var fd-var epfd &optional
			    (maxevents 1024)
			    (timeout -1))
		&body body)
  (let ((events (gensym "EVENTS-"))
	(e (gensym "E-"))
	(e-data (gensym "E-DATA-"))
	(i (gensym "I-"))
	(m (gensym "M-")))
    `(let ((,m ,maxevents))
       (with-foreign-object (,events '(:struct epoll-event) ,m)
	 (let ((,i (c-epoll-wait ,epfd ,events ,m ,timeout)))
	   (when (< ,i 0)
	     (error-errno "epoll_wait"))
	   (dotimes (,i ,i)
	     (let* ((,e (mem-aref ,events '(:struct epoll-event) ,i))
		    (,events-var (foreign-slot-value ,e '(:struct epoll-event) 'events))
		    (,e-data (foreign-slot-value ,e '(:struct epoll-event) 'data))
		    (,fd-var (foreign-slot-value ,e-data '(:union epoll-data) 'fd)))
	       ,@body)))))))
