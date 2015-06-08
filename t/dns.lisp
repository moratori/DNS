(in-package :cl-user)
(defpackage :dns-test
  (:use :cl
        :dns
        :prove))
(in-package :dns-test)

;; NOTE: To run this test file, execute `(asdf:test-system :dns)' in your Lisp.



(defun test1 ()
  (dns.resolver.stub:enquire 
    (make-instance 'dns.struct:a-record
                   :id 12345
                   :rd 1
                   :name "www.yahoo.co.jp")
    "8.8.8.8"))



(test1)
