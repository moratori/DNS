

(in-package :cl-user)
(defpackage dns.server.sender
  (:use :cl
        :cl-annot
        :usocket
        :dns.def.errors
        :dns.def.struct
        :dns.assemble.assemble
        )
  (:documentation 
    "DNSパケットの送信部分"))
(in-package :dns.server.sender)
(enable-annot-syntax)



@export
(defun send (host port dns-packet 
                  &key (timeout 1) (callback #'socket-close))
  (assert (typep dns-packet 'dns-packet))
  (let ((sock (socket-connect 
                host port 
                :protocol :datagram
                :timeout timeout))
        (buf (->raw dns-packet)))
    (socket-send sock buf (length buf))
    (funcall callback sock)))

