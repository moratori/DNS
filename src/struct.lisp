

(in-package :cl-user)
(defpackage dns.struct
  (:use :cl)
  (:export 
    :make-dns-header
    :dns-header
    :dns-header-id

    :dns-header-qr
    :dns-header-opcode
    :dns-header-aa
    :dns-header-tc
    :dns-header-rd
    :dns-header-ra
    :dns-header-z
    :dns-header-ad
    :dns-header-cd
    :dns-header-rcode

    :dns-header-qdcount
    :dns-header-ancount
    :dns-header-nscount
    :dns-header-arcount

    :make-dns-question
    :dns-question
    :dns-question-qname
    :dns-question-qtype
    :dns-question-qclass
    )
  )
(in-package :dns.struct)



(defstruct dns-header 
  "DNSヘッダ部分(12Byte)を表す構造体"
  (id 0 :type (integer 0 65535))

  (qr 0 :type bit)
  (opcode 0 :type (integer 0 15))
  (aa 0 :type bit)
  (tc 0 :type bit)
  (rd 0 :type bit)
  (ra 0 :type bit)
  (z  0 :type bit)
  (ad 0 :type bit)
  (cd 0 :type bit)
  (rcode 0 :type (integer 0 15))

  (qdcount 0 :type (integer 0 65535))
  (ancount 0 :type (integer 0 65535))
  (nscount 0 :type (integer 0 65535))
  (arcount 0 :type (integer 0 65535)))


(defstruct dns-question 
  (qname "" :type string)
  (qtype 0 :type  (integer 0 65535))
  (qclass 0 :type (integer 0 65535)))


