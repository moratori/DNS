

(in-package :cl-user)
(defpackage :dns.parse
  (:use :cl 
        :dns.struct
        :dns.errors
        :cl-annot
        :dns.util.bit
        )
  (:documentation 
    "(unsigned-byte 8)の配列を
     各セクションの構造体に変換する"))

(in-package :dns.parse)

(enable-annot-syntax)



@export
(defun parse-array-to-dns-packet (array)
  "(unsigned-byte 8)な配列をdns-packetに変換する"
  (declare (type (array (unsigned-byte 8) *) array))

  (format t "RECVED RAW ARRAY: ~%~A~%" array)

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
      (setf gp (parse-answer     array result gp ancount))
      (setf gp (parse-authority  array result gp nscount))
      (setf gp (parse-additional array result gp arcount)))
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


(defun %parse-name (array start)
  "startはドメイン名の格納が始まるarrayのインデックスを表す
   返り値は、ラベルのリストと次のセクションが始まるインデックス"
  (let* ((gp start)
        (name 
          (loop 
            for len = (aref array gp)
            while (< 0 len)
            collect 
            (let ((s (make-string len)))
              (incf gp)
              (loop 
                repeat len 
                for ch = (aref array gp)
                for si from 0
                do 
                  (setf (aref s si) (code-char ch))
                  (incf gp))
              s))))
    (values name (1+ gp))))


(defun %parse-question (array start)
  "arrayのstartから始まるQuestionセクションを
   dns-questionオブジェクトにして返す
   のと次のインデックスを返す"
  (multiple-value-bind 
    (name index) (%parse-name array start)
    (values 
      (make-dns-question 
        :qname  name 
        :qtype  (read-number array index 2)
        :qclass (read-number array (+ index 2) 2))
      (+ index 4)))) 


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


(defun %parse-rest (array start)
  "arrayのスタートから始まる、Answer,Authority,Additional
   の何れかのセクションをdns-restオブジェクトとして返す"
  (multiple-value-bind 
    (name index) (%parse-name array start)
    (let ((obj 
            (make-dns-rest 
              :name name 
              :type  (read-number array index 2)
              :class (read-number array (+ index 2) 2)
              :ttl   (read-number array (+ index 4) 4)
              :rdlength (read-number array (+ index 8) 2))))
      (let* ((len (dns-rest-rdlength obj))
             (rdata 
               (make-array len
                          :element-type '(unsigned-byte 8))))
        (loop 
          for gp from (+ index 10) below (+ index 10 len)
          for i from 0
          do (setf (aref rdata i) (aref array gp)))

        (setf (dns-rest-rdata obj) rdata)
        (values obj (+ index 10 len)))))) 


(defun %parse-aaa-section (array start count)
  "Answer,Authority,Additionalの何れも同じフォーマットなので
   それぞれリストにして返す"
  (let ((gp start))
    (values 
      (loop 
        repeat count 
        collect 
        (multiple-value-bind 
          (section next) 
          (%parse-rest array gp)
          (setf gp next)
          section))
        gp)))


(defun parse-answer (array result start ancount)
  (format t "ANSWER SECTION FROM HERE: ~A~%" start)
  (multiple-value-bind 
    (section next) (%parse-aaa-section array start ancount)
    (setf (dns-packet-answer result)
          section)
    next)) 

(defun parse-authority (array result start nscount)
  (multiple-value-bind 
    (section next) (%parse-aaa-section array start nscount)
    (setf (dns-packet-authority result)
          section)
    next))

(defun parse-additional (array result start arcount)
  (multiple-value-bind 
    (section next) (%parse-aaa-section array start arcount)
    (setf (dns-packet-additional result)
          section)
    next))

