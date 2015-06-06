
(in-package :cl-user)
(defpackage dns.util.assemble
  (:use :cl
        :dns.struct
        )
  (:export 
    :size
    :set-domain
    :domain-len
    :incremental-setf
    )
  )
(in-package :dns.util.assemble)



(defun set-domain (splited-domain array i)
  "arrayのi番目からdomain('www' 'yahoo' 'co' 'jp')を
   03 77 77 77 ... みたいなふうにsetする
   で次の書き込み開始indexを返す"
  (assert (typep array '(array (unsigned-byte 8))))
  (assert (typep splited-domain 'list))
  (let ((index i))
    (loop 
      for label in splited-domain
      do
        (setf (aref array index) (length label))
        (incf index)
        (loop 
          for ch across label
          do
            (setf (aref array index) (char-code ch))
            (incf index)))
    (setf (aref array index) 0)
    (1+ index)))


(defun domain-len (splited-domain)
  "domain名のために使う長さを返す"
  (+ 1
     (length splited-domain)
     (apply #'+ (mapcar #'length splited-domain))))
 



(defmethod size ((obj dns-packet))
  "DNSパケットを(unsigned-byte 8)配列に直すときに
   長さを求めるのに使う"
  (labels 
    ((sum (list)
       (reduce 
         #'+
         (mapcar #'size list)
         :initial-value 0)))
    (+
      (print (size (dns-packet-header obj)))
      (print (sum (dns-packet-question obj)))
      (print (sum (dns-packet-answer obj)))
      (print (sum (dns-packet-authority obj)))
      (print (sum (dns-packet-additional obj))))))


(defmethod size ((obj dns-header))
  "12Byteの固定長"
  12) 
 

(defmethod size ((obj dns-question))
  (+ 
    (domain-len (dns-question-qname obj))
    2 ; qtype len
    2 ; qclass len
    ))

(defmethod size ((obj dns-rest))
  (+ 
    (domain-len (dns-rest-name obj))
    2 ; type len
    2 ; class len
    2 ; ttl len
    2 ; rdatalen len
    (dns-rest-rdlength obj)))


