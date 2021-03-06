
(in-package :cl-user)
(defpackage :dns.server.templates
  (:use :cl 
        :dns.def.struct
        :dns.def.types
        :cl-annot
        )
  (:import-from 
    :cl-ppcre
    :split))
(in-package :dns.server.templates)

(enable-annot-syntax)



@export
(defun make-query (id rd domain qtype)
  "クエリを作る"
  (check-type id unsigned-short)
  (check-type rd bit)
  (check-type domain string)
  (check-type qtype unsigned-short)

  (make-dns-packet
    :header
     (make-dns-header
        :id id
        :qr 0
        :opcode 0
        :aa 0
        :tc 0
        :rd rd
        :ra 0
        :z  0
        :ad 0
        :cd 0
        :rcode 0
        :qdcount 1)
    :question
     (list 
        (make-dns-question
          :qname (split #\. domain)
          :qtype qtype
          :qclass 1))))



(defun make-Arecord-answer (id domain ipaddress) 
  "Aレコードを返す標準的なパケットを作る"
  (check-type id unsigned-short)
  (check-type domain string)
  (check-type qtype unsigned-short)
  (check-type ipaddress array)

 (make-dns-packet
    :header
     (make-dns-header
        :id id
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
         :rdata ipaddress)))) 

