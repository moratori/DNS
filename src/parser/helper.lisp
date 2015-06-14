
(in-package :cl-user)
(defpackage :dns.parser.helper
  (:use :cl
        :cl-annot
        :dns.def.struct
        :dns.def.errors
        :dns.util.bit
        )
  
  )
(in-package :dns.parser.helper)
(enable-annot-syntax)



(defun pointerp (len)
  "ラベル名長の上位2bitをみる"
  (= 3 (ldb (byte 2 6) len)))

(defun calc-jump-pointer (len next)
  (concat-bit 
    `((,(ldb (byte 6 0) len) 6)
      (,next 8))))

@export
(defun %parse-name (array start)
  "startはドメイン名の格納が始まるarrayのインデックスを表す
   返り値は、ラベルのリストと次のセクションが始まるインデックス"
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
          (multiple-value-bind
            (name _) 
            (%parse-name array (calc-jump-pointer len (aref array gp)))
            (declare (ignore _))
            (setf result (nconc (reverse name) result))
            (return-from exit)))
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
    (values (reverse result) (1+ gp))))


@export
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
 
@export
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


@export
(defun %parse-each-aaa (array start count)
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
 
