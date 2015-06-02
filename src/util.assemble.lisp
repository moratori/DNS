
(in-package :cl-user)
(defpackage dns.util.assemble
  (:use :cl)
  (:export 
    :set-domain
    :domain-len
    :incremental-setf
    )
  )
(in-package :dns.util.assemble)



(defmacro incremental-setf (index &rest body)
  `(setf 
     ,@(loop
         with result = nil
         with len = (length body)
         finally (return (nreverse result))
         for each in body
         for i from 0 
         do
           (push each result)
           (when (and (zerop (mod (1+ i) 2))
                      (< i (1- len)) )
             (push index result)
             (push `(1+ ,index) result)))))



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
 
