

(in-package :cl-user)
(defpackage dns.sender
  (:use :cl
        :usocket
        :dns.errors
        :dns.struct
        :dns.assemble
        )
  (:export 
    :send
    )
  (:documentation 
    "DNSパケットの送信部分"))
(in-package :dns.sender)




(defun send (host port dns-packet &key (timeout 1) (callback (lambda (s))))
  (assert (typep dns-packet 'dns-packet))
  (let ((sock (socket-connect 
                host port 
                :protocol :datagram
                :timeout timeout))
        (buf (->raw dns-packet)))
    (socket-send sock buf (length buf))
    (funcall callback sock)
    (socket-close sock)))
