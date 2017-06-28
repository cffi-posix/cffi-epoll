;;
;;  cffi-epoll  -  Common Lisp wrapper for Linux epoll syscall
;;
;;  Copyright 2017 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

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

(cunion epoll-data "union epoll_data"
  (ptr "ptr" :type :pointer)
  (fd "fd" :type :int)
  (u32 "u32" :type uint32-t)
  (u64 "u64" :type uint64-t))

(cstruct epoll-event "struct epoll_event"
  (events "events" :type uint32-t)
  (data "data" :type (:union epoll-data)))
