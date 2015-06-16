

(in-package :cl-user)
(defpackage :dns.server.resolver.stub
  (:use :cl 
        :cl-annot
        :usocket
        :dns.def.struct
        :dns.def.types
        :dns.server.sender
        :dns.server.templates
        :dns.parser.parser
        ))

(in-package :dns.server.resolver.stub)
(enable-annot-syntax)


(defun handler (q socket)
  "DNS問い合わせをしたあとに、応答を処理する関数"
  (check-type q query)
  
  (let* ((len 512)
         (dns-packet-array 
           (make-array 
             len
             :element-type '(unsigned-byte 8)))) 
    (socket-receive socket dns-packet-array len)
    (socket-close socket)
    (parse-array-to-dns-packet dns-packet-array))) 


@export
(defun enquire (query-format cache-server &key (port 53) (timeout 1))
  "query-formatで指定されたDNSパケットつくって
   cache-serverに投げる関数"
  (check-type query-format query)
  (check-type cache-server string)
  (check-type port unsigned-short)
  (check-type timeout integer)

  (send 
    cache-server 
    port 
    (enquire-packet query-format)
    :timeout timeout
    :callback (lambda (s) (handler query-format s)))) 



(defgeneric enquire-packet (qf)
  (:documentation 
    "していされた形式のクエリを作成するメソッド"))


(defmethod enquire-packet ((q a-record))
  (make-query 
    (id q) (rd q) (name q) 1))


(defmethod enquire-packet ((q ns-record))
  
  )

(defmethod enquire-packet ((q query))
  (error "query not implemented"))


