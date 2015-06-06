

(in-package :cl-user)
(defpackage dns.struct
  (:use :cl
        :cl-annot
        :cl-annot.class) 
  (:documentation 
    "DNSパケットの定義"
    )
  )
(in-package :dns.struct)

(enable-annot-syntax)


@export-structure
(defstruct dns-packet 
  "5つのセクションからなるDNSパケット"
  (header nil :type dns-header)
  ;; following slot type is (list dns-question)
  (question nil :type   list)
  ;; following slots type is (list dns-rest)
  (answer nil :type     list)
  (authority nil :type  list)
  (additional nil :type list))

@export-structure
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

@export-structure
(defstruct dns-question 
  "Questionセクションを表す構造体"
  (qname nil :type list)
  (qtype 0 :type  (integer 0 65535))
  (qclass 0 :type (integer 0 65535)))

@export-structure
(defstruct dns-rest
  "Answer,Authority,Additionalの各セクションのフォーマットは全て同じであるため
   この構造体でそれらを表す"
  (name nil :type list)
  (type 0 :type (integer 0 65535))
  (class 0 :type (integer 0 65535))
  (ttl 0 :type (integer 0 65535))
  (rdlength 0 :type (integer 0 65535))
  (rdata "" :type array))


