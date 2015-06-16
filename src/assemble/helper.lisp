

(in-package :cl-user)
(defpackage dns.assemble.helper
  (:use :cl
        :dns.def.struct
        :dns.def.types
        :cl-annot
        )
  )
(in-package :dns.assemble.helper)


(enable-annot-syntax)


@export
(defun set-domain (splited-domain array i)
  "arrayのi番目からdomain('www' 'yahoo' 'co' 'jp')を
   03 77 77 77 ... みたいなふうにsetする
   で次の書き込み開始indexを返す"
  (check-type array dns-packet-array)
  (check-type i     unsigned-short)
  (assert (and (listp splited-domain)
               (every #'stringp splited-domain)))

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
  (assert (and (listp splited-domain)
               (every #'stringp splited-domain)))

  (+ 1
     (length splited-domain)
     (apply #'+ (mapcar #'length splited-domain))))
 


@export
(defgeneric size (object)
  (:method ((object dns-header))
    ;; dns ヘッダの長さは固定長
    12)
  (:documentation 
    "各セクションの長さやパケットの大きさを返す"))


(defmethod size ((obj dns-packet))
  (labels 
    ((sum (list)
       (reduce 
         #'+
         (mapcar #'size list)
         :initial-value 0)))
    (+
      (size (dns-packet-header obj))
      (sum (dns-packet-question obj))
      (sum (dns-packet-answer obj))
      (sum (dns-packet-authority obj))
      (sum (dns-packet-additional obj)))))


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
    4 ; ttl len
    2 ; rdatalen len
    (dns-rest-rdlength obj)))


