
(in-package :cffi-epoll)

(include "sys/epoll.h")

(ctype uint32-t "uint32_t")
(ctype uint64-t "uint64_t")

(constant (+ctl-add+ "EPOLL_CTL_ADD"))
(constant (+ctl-mod+ "EPOLL_CTL_MOD"))
(constant (+ctl-del+ "EPOLL_CTL_DEL"))

(constant (+in+        "EPOLLIN"))
(constant (+out+       "EPOLLOUT"))
(constant (+rdhup+     "EPOLLRDHUP"))
(constant (+pri+       "EPOLLPRI"))
(constant (+err+       "EPOLLERR"))
(constant (+hup+       "EPOLLHUP"))
(constant (+et+        "EPOLLET"))
(constant (+exclusive+ "EPOLLEXCLUSIVE"))
