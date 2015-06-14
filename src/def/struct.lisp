

(in-package :cl-user)
(defpackage dns.def.struct
  (:use :cl
        :cl-annot
        :cl-annot.class) 
  (:documentation 
    "DNSパケットの定義"
    )
  )
(in-package :dns.def.struct)

(enable-annot-syntax)


@export-structure
(defstruct dns-packet 
  "5つのセクションからなるDNSパケット"
  (header nil :type (or null dns-header))
  (question nil :type   list)
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

(defun dns-header-accessors ()
  (mapcar 
    (lambda (tip)
      (symbol-function 
        (intern (string-upcase (concatenate 'string "dns-header-" tip)))))
    (list 
      "id" "qr" "opcode" "aa" "tc" "rd" "ra"
      "z" "ad" "cd" "rcode" "qdcount" "ancount" "nscount" "arcount")))



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
  (ttl 0 :type (integer 0 4294967295))
  (rdlength 0 :type (integer 0 65535))
  (rdata nil :type (or null (array (unsigned-byte 8)))))





 
@export
(defmethod packet= ((p1 t) (p2 t))
  nil)

(defmethod packet= ((p1 dns-packet) (p2 dns-packet))
  (and 
    (or (and (null (dns-packet-header p1))
             (null (dns-packet-header p1)))
        (packet= p1 p2))
    (every 
      (lambda (f)
        (every 
          #'packet= 
          (funcall f p1)
          (funcall f p2)))
      (list 
        #'dns-packet-question 
        #'dns-packet-answer 
        #'dns-packet-authority 
        #'dns-packet-additional))))

(defmethod packet= ((p1 dns-header) (p2 dns-header))
  (every 
    (lambda (f)
      (= (funcall f p1) (funcall f p2)))
    (dns-header-accessors)))
 

(defmethod packet= ((p1 dns-question) (p2 dns-question))
  (and 
    (every #'string= (dns-question-name p1) (dns-question-name p2))
    (= (dns-question-qtype p1) (dns-question-qtype p2))
    (= (dns-question-qclass p1) (dns-question-qclass p2))))

(defmethod packet= ((p1 dns-rest) (p2 dns-rest))
  (and 
    (every #'string= 
           (dns-rest-name p1)
           (dns-rest-name p2))
    (every 
      (lambda (f) (= (funcall f p1) (funcall fp2)))
      (list 
        #'dns-rest-type 
        #'dns-rest-class 
        #'dns-rest-ttl 
        #'dns-rest-rdlength))
    (equal (dns-rest-rdata p1) (dns-rest-rdata p2))))



@export 
@export-accessors
(defclass query ()
  ((id 
     :initform 0
     :initarg :id 
     :reader id)
   (rd 
     :initform t 
     :initarg :rd 
     :reader rd)
   (name  
     :initform "."
     :initarg  :name 
     :reader name)))

@export
(defclass a-record (query)
  ())


@export 
(defclass ns-record (query)
  ())

