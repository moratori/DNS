
(in-package :cl-user)
(defpackage :dns.util.bit
  (:use :cl
        :cl-annot
        )
  )
(in-package :dns.util.bit)

(enable-annot-syntax)


@export 
(defun read-number (array start size)
  "unsigned-byte 8 の配列arrayのstartからsize 
   分だけ読んで整数にして返す"
  (assert (typep array '(array (unsigned-byte 8) *)))
  (concat-bit 
    (loop 
      for index from start below (+ start size) 
      collect (list (aref array index) 8)))) 


(defun split-16-to-88 (n)
  "16bit以内で表されるfixnumを1Byteの数２つで表す
   5346 => 20; 226"
  (assert (< -1 n 65536))
  (values 
    (ldb (byte 8 8) n)
    (ldb (byte 8 0) n)))

(defun integer->bit-list (n)
  "整数を、2進数を表す1,0のリストにする
   15 => (1 1 1 1)"
  (assert (< -1 n))
  (let (r)
    (loop
      named exit
      with s = n
      when (< s 2) do 
      (progn 
        (push s r)
        (return-from exit))
      do (multiple-value-bind 
           (a b) (truncate s 2)
           (setf s a)
           (push b r)))
    r))


(defun bit-list->integer (bit-list)
  "二進数を表す1,0のリストを整数にする
   (1 1 1 1) => 15"
  (loop 
    for i from (1- (length bit-list)) downto 0
    for each in bit-list
    summing (* each (expt 2 i)))) 

@export
(defun set-16 (n array index)
  "16bitで表される数nを1Byte毎に分割して
   それぞれを配列のindex,index+1で表される場所に格納"
  (assert (< -1 n 65536))
  (multiple-value-bind 
    (a b) (split-16-to-88 n)
    (setf (aref array index) a)
    (setf (aref array (1+ index)) b)))


(defun integer->bit-list-with-size (sized-int)
  (destructuring-bind 
    (int size) sized-int
    (let ((r (integer->bit-list int)))
    (when (< (length r) size)
      (loop 
        repeat (- size (length r))
        do (push 0 r)))
    r))) 

@export
(defun concat-bit (sized-int) 
  "引数に与えられた整数を並べた時にできる数を返す
   (concat-bit 1 2 3) -> 1 10 11 => 27"
  (bit-list->integer 
    (mapcan 
      #'integer->bit-list-with-size 
      sized-int)))
