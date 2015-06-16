
(in-package :cl-user)
(defpackage :dns.parser.helper
  (:use :cl
        :cl-annot
        :dns.def.struct
        :dns.def.types
        :dns.def.errors
        :dns.util.bit))
(in-package :dns.parser.helper)
(enable-annot-syntax)



(defun pointerp (len)
  "ラベル名長の上位2bitをみる"
  (check-type len octet)
  
  (= 3 (ldb (byte 2 6) len)))

(defun calc-jump-pointer (len next)
  "lenの上位2bitが11である場合は次の1Byteの値(next)
   とlenの下位6bitでポインタを表しているのでそれを求める"
  (check-type len  octet)
  (check-type next octet)
  
  (concat-bit 
    `((,(ldb (byte 6 0) len) 6)
      (,next 8))))


(defun check-length (splited-domain)
  "ドメイン名が長253以下であるか"
  (assert (and (listp splited-domain)
               (every #'stringp splited-domain)))

  (let ((comma-len (1- (length splited-domain)))
        (len (loop for each in splited-domain summing (length each))))
    (unless 
      (<= (+ comma-len len) 253)
      (error 
        (make-condition malformed-name
          :msg "name too long")))))


@export
(defun %parse-name (array start)
  "startはドメイン名の格納が始まるarrayのインデックスを表す
   返り値は、ラベルのリストと次のセクションが始まるインデックス"
  (check-type array dns-packet-array)
  (check-type start unsigned-short)
  
  (let* ((gp start)
         (result nil))
    (loop 
      named exit
      for len = (aref array gp)
      while (< 0 len)
      do 
      (if (pointerp len)
        (progn 
          (incf gp)
          (let ((jmpptr (calc-jump-pointer len (aref array gp))))
            
            (unless (< 11 jmpptr start)
              (error 
                (make-condition 
                  'malformed-name 
                  :msg "Pointer is referring to an unusual location")))
            
            (multiple-value-bind
              (name _) 
              (%parse-name array jmpptr)
              (declare (ignore _))
              (setf result (nconc (reverse name) result))
              (return-from exit))))

        (let ((s (make-string len)))
          (incf gp)
          (loop 
            repeat len 
            for ch = (aref array gp)
            for si from 0
            do 
              (setf (aref s si) (code-char ch))
              (incf gp))
          (push s result))))
    (check-length result)
    (values (reverse result) (1+ gp))))


@export
(defun %parse-question (array start)
  "arrayのstartから始まるQuestionセクションを
   dns-questionオブジェクトにして返す
   のと次のインデックスを返す"
  (check-type array dns-packet-array)
  (check-type start unsigned-short)

  (multiple-value-bind 
    (name index) (%parse-name array start)
    (values 
      (make-dns-question 
        :qname  name 
        :qtype  (read-number array index 2)
        :qclass (read-number array (+ index 2) 2))
      (+ index 4)))) 
 
@export
(defun %parse-rest (array start)
  "arrayのスタートから始まる、Answer,Authority,Additional
   の何れかのセクションをdns-restオブジェクトとして返す"
  (check-type array dns-packet-array)
  (check-type start unsigned-short)

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


@export
(defun %parse-each-aaa (array start count)
  "Answer,Authority,Additionalの何れも同じフォーマットなので
   それぞれリストにして返す"
  (check-type array dns-packet-array)
  (check-type start unsigned-short)
  (check-type count unsigned-short)

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
 
