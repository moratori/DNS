

(in-package :cl-user)
(defpackage :dns.resolver.stub
  (:use :cl
        :dns.struct
        :dns.sender
        :usocket
        :dns.parse
        :dns.sender
        :dns.templates
        :cl-annot)
  )

(in-package :dns.resolver.stub)
(enable-annot-syntax)



(defun handler (q socket)
  (let* ((len 512)
         (dns-packet-array 
           (make-array 
             len
             :element-type '(unsigned-byte 8)))) 
    (socket-receive socket dns-packet-array len)
    (socket-close socket)
    (print (parse-array-to-dns-packet dns-packet-array))
    
    ))


@export
(defun enquire (query-format cache-server &key (port 53) (timeout 1))
  (send 
    cache-server 
    port 
    (enquire-packet query-format)
    :timeout timeout
    :callback (lambda (s) (handler query-format s)))) 



(defmethod enquire-packet ((q a-record))
  (make-query 
    (id q) (rd q) (name q) 1))


(defmethod enquire-packet ((q ns-record))
  
  )


(defmethod enquire-packet ((q query))
  (error "query not implemented"))


