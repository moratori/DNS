
(in-package :cl-user)
(defpackage dns.errors
  (:use :cl
        ))
(in-package :dns.errors)



(define-condition malformed ()
  ()
  )


(define-condition malformed-header ()
  ((msg 
     :initform ""
     :initarg :msg
     :accessor msg))) 

