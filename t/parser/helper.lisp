
(in-package :cl-user)
(defpackage :dns.parser.helper-test
  (:use :cl 
        :prove
        :dns.parser.helper
        )
  )
(in-package :dns.parser.helper-test)



(defvar *name* 
  (mapcar 
    (lambda (e)
      (make-array (length e) 
                  :element-type '(unsigned-byte 8) 
                  :initial-contents e))
    '(
      (4 106 112 114 115 2 106 112 0)
      (
       0 0 0 0 0 0 0 0 0 0 0 0
       2 106 112 0 
       4 106 112 114 115 
       192 12)
      (
       0 0 0 0 0 0 0 0 0 0 0 0
       3 100 110 115 
       2 106 112 
       0 
       1 97 
       192 12)
      (
       0 0 0 0 0 0 0 0 0 0 0 0
       1 2 3 4 
       2 106 112 
       0 
       1 97 
       3 100 110 115 
       192 16) ))) 


(defvar *start* 
  '(0 16 20 20))

(defvar *answer* 
  '(("jprs" "jp") 
    ("jprs" "jp")
    ("a" "dns" "jp")
    ("a" "dns" "jp"))) 



(plan (+ (length *name*) 0))


(subtest "ドメイン名をパースできるかテスト"
  (loop 
  for test in *name*
  for ans in *answer*
  for start in *start*
  do 
  (ok (equal (%parse-name test start) ans))))

(finalize)




