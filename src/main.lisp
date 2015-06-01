
(in-package :cl-user)
(defpackage dns.main
  (:use :cl
        :usocket
        :dns.errors
        :dns.struct
        :dns.util
        )
  (:import-from 
    :cl-ppcre
    :split)
  (:documentation 
    "DNSパケットの組み立てを行う"
    )
  )
(in-package :dns.main)

(setf *random-state* (make-random-state t))


(defmethod ->raw-section ((obj dns-header))
  "dns-headerオブジェクトから送信可能な(unsigned-byte 8)配列(dnsヘッダ部分のみ)を作って返す"
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



(defmethod ->raw-section ((obj dns-question))
  "dns-questionオブジェクトからquestionセクションを作成する"
  (let* ((name (dns-question-qname obj))
         (type (dns-question-qtype obj))
         (class (dns-question-qclass obj))
         (sp   (split #\. name))
         (len 
           (+ (apply #'+ (mapcar #'length sp)) (length sp) 1 4))
         (result 
           (make-array len 
             :element-type '(unsigned-byte 8)))) 
    (let ((i 0))
      (loop for each in sp
            do 
            (progn 
              (setf (aref result i) (length each))
              (incf i)
              (loop for ch across each 
                    do (progn 
                         (setf (aref result i) (char-code ch))
                         (incf i)))))
      (setf 
        (aref result i) 0
        i (+ i 1)
        (aref result i) (ldb (byte 8 8) type)
        i (+ i 1)
        (aref result i) (ldb (byte 8 0) type)
        i (+ i 1)
        (aref result i) (ldb (byte 8 8) class)
        i (+ i 1)
        (aref result i) (ldb (byte 8 0) class)))
    result))



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



(defun make-standard-query (domain)
  (concat-all-section
    (->raw-section
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
        :qdcount 1))
    (->raw-section
      (make-dns-question
        :qname domain
        :qtype 1
        :qclass 1))))



(defun send (host)
  (let ((q (make-standard-query "www.yahoo.co.jp"))
        (s (socket-connect host 53 :protocol :datagram)))

    (socket-send s q (length q))
    (sleep 1)
    (socket-close s)))




