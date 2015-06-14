(in-package :cl-user)
(defpackage :dns-test
  (:use :cl
        :dns
        :dns.def.struct
        :prove))
(in-package :dns-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dns)' in your Lisp.
;;
;;

(setf 
  *random-state* (make-random-state t)
  prove:*default-reporter* :fiveam) 
 

(plan 0)

(finalize)

