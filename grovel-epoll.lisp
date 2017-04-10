
(in-package :cffi-epoll)

(include "sys/epoll.h")

(ctype uint32-t "uint32_t")
(ctype uint64-t "uint64_t")

(constant (+epoll-ctl-add+ "EPOLL_CTL_ADD"))
(constant (+epoll-ctl-mod+ "EPOLL_CTL_MOD"))
(constant (+epoll-ctl-del+ "EPOLL_CTL_DEL"))

(constant (+epoll-in+    "EPOLLIN"))
(constant (+epoll-out+   "EPOLLOUT"))
(constant (+epoll-rdhup+ "EPOLLRDHUP"))
(constant (+epoll-pri+   "EPOLLPRI"))
(constant (+epoll-err+   "EPOLLERR"))
(constant (+epoll-hup+   "EPOLLHUP"))
(constant (+epoll-et+    "EPOLLET"))
(constant (+epoll-exclusive+ "EPOLLEXCLUSIVE"))
