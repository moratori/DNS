
(in-package :cl-user)
(defpackage dns.def.errors
  (:use :cl
        :cl-annot
        ))
(in-package :dns.def.errors)
(enable-annot-syntax)


(define-condition malformed (error)
  ((msg 
     :initform ""
     :initarg :msg
     :accessor msg)))

@export
(define-condition malformed-header (malformed)
  ()) 

@export 
(define-condition malformed-name (malformed)
  ()
  )
