

(in-package :cl-user)
(defpackage dns.parser.parser
  (:use :cl 
        :cl-annot
        :dns.def.struct
        :dns.def.errors
        :dns.util.bit
        :dns.parser.helper
        )
  (:documentation 
    "(unsigned-byte 8)の配列を
     dns-packetの構造体に変換する"))

(in-package :dns.parser.parser)

(enable-annot-syntax)



@export
(defun parse-array-to-dns-packet (array)
  "(unsigned-byte 8)な配列をdns-packetに変換する"
  (declare (type (array (unsigned-byte 8) *) array))

  (let ((len (length array))
        (result (make-dns-packet)))

    (when (< len 12)
      (error 
        (make-condition 
          'malformed-header 
          :msg (format nil "header too short: ~D" len))))
    
    (let* ((gp (parse-header array result 0 1))
           (header  (dns-packet-header result))
           (qdcount (dns-header-qdcount header))
           (ancount (dns-header-ancount header))
           (nscount (dns-header-nscount header))
           (arcount (dns-header-arcount header)))

      (setf gp (parse-quetion    array result gp qdcount))

      (loop 
        for (f . cnt) in `((dns-packet-answer . ,ancount)
                           (dns-packet-authority . ,nscount)
                           (dns-packet-additional . ,arcount))
        do (setf gp (parse-aaa-section f array result gp cnt)) )) 

    result))


(defun parse-header (array result start hcnt)
  "配列arrayをdns-headerオブジェクトにして
   resultにセットする
   qdcount > 0であるならば parse-quetionに渡す"
  (declare (ignore start hcnt))
  (let ((left  (aref array 2))
        (right (aref array 3)))
    (setf (dns-packet-header result)
          (make-dns-header
            :id (read-number array 0 2) 

            :qr     (ldb (byte 1 7) left)
            :opcode (ldb (byte 4 3) left)
            :aa     (ldb (byte 1 2) left)
            :tc     (ldb (byte 1 1) left)
            :rd     (ldb (byte 1 0) left)

            :ra    (ldb (byte 1 7) right)
            :z     (ldb (byte 1 6) right)
            :ad    (ldb (byte 1 5) right)
            :cd    (ldb (byte 1 4) right)
            :rcode (ldb (byte 4 0) right)

            :qdcount (read-number array 4 2)
            :ancount (read-number array 6 2)
            :nscount (read-number array 8 2)
            :arcount (read-number array 10 2))))
  12)



(defun parse-quetion (array result start qcnt)
  (let ((gp start))
    (setf 
      (dns-packet-question result)
      (loop 
        repeat qcnt 
        collect 
        (multiple-value-bind 
          (quetion next) 
          (%parse-question array gp)
          (setf gp next)
          quetion)))
    gp
    ))


(defun parse-aaa-section (f array result start cnt)
  (multiple-value-bind 
    (section next)
    (%parse-each-aaa array start cnt)
    (funcall (fdefinition `(setf ,f)) section result)
    next))


