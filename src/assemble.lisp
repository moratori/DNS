
(in-package :cl-user)
(defpackage dns.assemble
  (:use :cl
        :usocket
        :dns.errors
        :dns.struct
        :dns.util.bit
        )
  (:import-from 
    :cl-ppcre
    :split)
  (:import-from
    :dns.util.assemble
    :set-domain
    :domain-len
    :incremental-setf
    )
  (:export 
    :concat-all-section
    :->raw
    )
  (:documentation 
    "DNSパケットの組み立てを行う"
    )
  )
(in-package :dns.assemble)








(defmethod ->raw ((obj dns-packet))
  "DNSパケットを送信可能な配列にする"
  (labels 
    ((inner-concat (list)
       (if (null list)
         #()
         (reduce 
           (lambda (result x) 
             (concat-all-section result x)) 
           (mapcar #'->raw list)))))
    (concat-all-section
      (->raw (dns-packet-header obj))
      (inner-concat (dns-packet-question obj))
      (inner-concat (dns-packet-answer   obj))
      (inner-concat (dns-packet-authority obj))
      (inner-concat (dns-packet-additional obj)))))



(defmethod ->raw ((obj dns-header))
  "dns-header構造体から送信可能な(unsigned-byte 8)配列を作って返す"
  (let ((result 
          (make-array 
            12 :element-type '(unsigned-byte 8)))) 
    (set-16 (dns-header-id obj) result 0)
    (set-16 (dns-header-qdcount obj) result 4)
    (set-16 (dns-header-ancount obj) result 6)
    (set-16 (dns-header-nscount obj) result 8)
    (set-16 (dns-header-arcount obj) result 10)
    (setf (aref result 2) 
          (concat-bit 
            (dns-header-qr obj)
            (dns-header-opcode obj)
            (dns-header-aa obj)
            (dns-header-tc obj)
            (dns-header-rd obj)))
    (setf (aref result 3) 
          (concat-bit 
            (dns-header-ra obj)
            (dns-header-z obj)
            (dns-header-ad obj)
            (dns-header-cd obj)
            (dns-header-rcode obj)))
    result))



(defmethod ->raw ((obj dns-question))
  "dns-question構造体から送信可能な(unsigned-byte 8)配列を作って返す"
  (let* ((name (dns-question-qname obj))
         (type (dns-question-qtype obj))
         (class (dns-question-qclass obj))
         (sp   (split #\. name))
         (result 
           (make-array (+ (domain-len sp) 4)
             :element-type '(unsigned-byte 8))))  
    (let ((i (set-domain sp result 0)))
      (incremental-setf i
        (aref result i) (ldb (byte 8 8) type)
        (aref result i) (ldb (byte 8 0) type)
        (aref result i) (ldb (byte 8 8) class)
        (aref result i) (ldb (byte 8 0) class)))
    result))



(defmethod ->raw ((obj dns-rest))
  "dns-rest構造体から送信可能な(unsigned-byte 8)配列を作って返す"

  )




(defun concat-all-section (&rest args)
  "各セクションをくっつけて一つのdnsパケットを作る"
  (let* ((len (apply #'+ (mapcar #'length args)))
         (result (make-array len :element-type '(unsigned-byte 8))))
    (let ((i 0))
      (loop for array in args
            do 
            (loop for elm across array
                  do
                  (setf (aref result i) elm)
                  (incf i))))
    result))
 


