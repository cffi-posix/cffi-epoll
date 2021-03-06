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
   #:+ctl-add+
   #:+ctl-del+
   #:+ctl-mod+
   #:+err+
   #:+et+
   #:+exclusive+
   #:+hup+
   #:+in+
   #:+out+
   #:+pri+
   #:+rdhup+
   #:add
   #:c-epoll-create
   #:c-epoll-ctl
   #:c-epoll-wait
   #:create
   #:ctl
   #:del
   #:mod
   #:wait
   #:with
   ))
