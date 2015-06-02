#|
  This file is a part of dns project.
  Copyright (c) 2015 moratori (teldev@live.jp)
|#

#|
  Author: moratori (teldev@live.jp)
|#

(in-package :cl-user)
(defpackage dns-asd
  (:use :cl :asdf))
(in-package :dns-asd)

(defsystem dns
  :version "0.1"
  :author "moratori"
  :license "LLGPL"
  :depends-on (:usocket :cl-ppcre :cl-annot)
  :components ((:module "src"
                :components
                (
(:file "errors")
(:file "struct")
(:file "util.bit")
(:file "util.assemble")
(:file "assemble")
(:file "resolver.stub")
(:file "parse")
(:file "sender")
(:file "dns")

)))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op dns-test))))
