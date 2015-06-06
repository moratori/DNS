
(in-package :cl-user)
(defpackage dns.assemble
  (:use :cl
        :usocket
        :dns.errors
        :dns.struct
        :dns.util.assemble
        :dns.util.bit
        )
  
  (:export 
    :->raw
    )
  (:documentation 
    "DNSパケットの組み立てを行う"
    )
  )
(in-package :dns.assemble)





(defun ->raw (dns-packet)
  "DNSパケットを送信可能な(unsigned 8)配列にして返す"
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
      (setf i (write-all (dns-packet-question dns-packet) result i))
      (setf i (write-all (dns-packet-answer   dns-packet) result i))
      (setf i (write-all (dns-packet-authority dns-packet) result i))
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
            (dns-header-qr obj)
            (dns-header-opcode obj)
            (dns-header-aa obj)
            (dns-header-tc obj)
            (dns-header-rd obj)))
    (setf (aref result (+ start 3)) 
          (concat-bit 
            (dns-header-ra obj)
            (dns-header-z obj)
            (dns-header-ad obj)
            (dns-header-cd obj)
            (dns-header-rcode obj))) 
    12)

(defmethod %write-section ((obj dns-question) result start)
  (let* ((name (dns-question-qname obj))
         (type (dns-question-qtype obj))
         (class (dns-question-qclass obj)))  
    (let ((i (set-domain name result start)))
      (set-16 type result i)
      (incf i 2)
      (set-16 class result i)
      (+ i 2))))



(defmethod %write-section ((obj dns-rest) result start)
  (let ((name (dns-rest-name obj))
        (type (dns-rest-type obj))
        (class (dns-rest-class obj))
        (ttl (dns-rest-ttl obj))
        (rdlength (dns-rest-rdlength obj))
        (rdata (dns-rest-rdata obj)))
    (let ((i (set-domain name result start)))
      (set-16 type result i)
      (incf i 2)
      (set-16 class result i)
      (incf i 2)
      (set-16 ttl result i)
      (incf i 2)
      (set-16 rdlength result i)
      (incf i 2)
      (loop 
        with index = i
        finally (return index)
        for ch across rdata
        do 
          (setf (aref result index) ch)
          (incf index)))))


