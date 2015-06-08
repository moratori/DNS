
(in-package :cl-user)
(defpackage dns.assemble
  (:use :cl
        :usocket
        :dns.errors
        :dns.struct
        :dns.util.assemble
        :dns.util.bit
        :cl-annot
        )
  
  (:documentation 
    "DNSパケットの組み立てを行う"))
(in-package :dns.assemble)
(enable-annot-syntax)


@export
(defun ->raw (dns-packet)
  "DNSパケットを送信可能な(unsigned-byte 8)配列にして返す"
  (assert (typep dns-packet 'dns-packet))
  (let* ((result 
          (make-array 
            (size dns-packet)
            :element-type '(unsigned-byte 8)))
         (i (%write-section (dns-packet-header dns-packet) result 0)))
    (labels 
      ((write-all (list array start)
         (loop 
           with i = start
           finally (return i)
           for each in list
           do (setf i (%write-section each array i)))))
      (setf i (write-all (dns-packet-question dns-packet)   result i))
      (setf i (write-all (dns-packet-answer   dns-packet)   result i))
      (setf i (write-all (dns-packet-authority dns-packet)  result i))
      (setf i (write-all (dns-packet-additional dns-packet) result i)))
    result))

(defgeneric %write-section (dns-header-obj array start-index)
  (:documentation 
    "DNSヘッダや各セクションの構造体を(unsigned-byte 8)配列に変換し
     start-indexで表されるところからarrayに書き込んで行く"))

(defmethod %write-section ((obj dns-header) result start)
    (set-16 (dns-header-id obj) result start)
    (set-16 (dns-header-qdcount obj) result (+ start 4))
    (set-16 (dns-header-ancount obj) result (+ start 6))
    (set-16 (dns-header-nscount obj) result (+ start 8))
    (set-16 (dns-header-arcount obj) result (+ start 10))
    (setf (aref result (+ start 2)) 
          (concat-bit 
            `((,(dns-header-qr obj) 1)
              (,(dns-header-opcode obj) 4) 
              (,(dns-header-aa obj) 1)
              (,(dns-header-tc obj) 1)
              (,(dns-header-rd obj) 1))))
    (setf (aref result (+ start 3)) 
          (concat-bit 
            `((,(dns-header-ra obj) 1)
              (,(dns-header-z obj) 1) 
              (,(dns-header-ad obj) 1)
              (,(dns-header-cd obj) 1)
              (,(dns-header-rcode obj) 4)))) 
    12)

(defmethod %write-section ((obj dns-question) result start)
  (let ((i (set-domain (dns-question-qname obj) result start)))
    (set-16 (dns-question-qtype obj)  result i)
    (set-16 (dns-question-qclass obj) result (+ i 2))
    (+ i 4)))

(defmethod %write-section ((obj dns-rest) result start)
  (let ((ttl (dns-rest-ttl obj))
        (i (set-domain (dns-rest-name obj) result start)))
    (set-16 (dns-rest-type obj) result i)
    (set-16 (dns-rest-class obj) result (+ i 2))
    (set-16 (ldb (byte 16 16) ttl) result (+ i 4))
    (set-16 (ldb (byte 16 0) ttl) result (+ i 6))
    (set-16 (dns-rest-rdlength obj) result (+ i 8))
    (loop 
      with index = (+ i 10)
      finally (return index)
      for ch across (dns-rest-rdata obj)
      do 
        (setf (aref result index) ch)
        (incf index))))

