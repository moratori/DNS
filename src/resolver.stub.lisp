

(in-package :cl-user)
(defpackage :dns.resolver.stub
  (:use :cl
        :dns.errors
        :usocket
        :dns.struct
        :dns.assemble)
  (:export
    :make-Arecord-query
    )
  )
(in-package :dns.resolver.stub)


(setf *random-state* (make-random-state t))



(defun make-Arecord-query (domain)
  "domainについてAレコードを問い合わせる標準的なクエリを作る"
  (make-dns-packet
    :header
     (make-dns-header
        :id (random 65535)
        :qr 0
        :opcode 0
        :aa 0
        :tc 0
        :rd 1
        :ra 0
        :z  0
        :ad 0
        :cd 0
        :rcode 0
        :qdcount 1)
    :question
     (list 
        (make-dns-question
          :qname domain
          :qtype 1
          :qclass 1))))


