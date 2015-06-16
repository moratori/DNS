
(in-package :cl-user)
(defpackage :dns.def.types
  (:use :cl
        :cl-annot)
  )
(in-package :dns.def.types)
(enable-annot-syntax)


@export 
(deftype octet () 
  '(unsigned-byte 8))

@export 
(deftype unsigned-short ()
  '(integer 0 65535))

@export 
(deftype dns-packet-array ()
  '(simple-array (unsigned-byte 8) *))

