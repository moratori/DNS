

(in-package :cl-user)
(defpackage :dns.resolver.full
  (:use :cl
        :dns.errors
        :dns.struct
        :dns.assemble )
  (:import-from 
    :cl-ppcre
    :split
    )
  )
(in-package :dns.resolver.full)


(defun make-Arecord-answer (domain ipaddress) 
(make-dns-packet
    :header
     (make-dns-header
        :id (random 65535)
        :qr 1
        :opcode 0
        :aa 0
        :tc 0
        :rd 1
        :ra 1
        :z  0
        :ad 0
        :cd 0
        :rcode 0
        :qdcount 1
        :ancount 1
        )
    :question
     (list 
        (make-dns-question
          :qname (split #\. domain)
          :qtype 1
          :qclass 1))
     :answer 
     (list 
       (make-dns-rest 
         :name (split #\. domain)
         :type 1
         :class 1
         :ttl 3600
         :rdlength 4
         :rdata #(192 168 1 1)
         )
       )
     )
  )
