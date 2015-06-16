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
                ((:module "def" 
                  :components 
                  ((:file "errors")
                   (:file "types")
                   (:file "struct")))

                 (:module "util"
                  :components 
                  ((:file "bit")))

                 (:module "assemble"
                  :components 
                  ((:file "helper")
                   (:file "assemble")))
                 
                 (:module "parser"
                  :components 
                  ((:file "helper")
                   (:file "parser"))) 

                 (:module "server"
                  :components 
                  ((:file "sender")
                   (:file "templates")
                   (:module "authoriative"
                    :components 
                    ())
                   (:module "resolver"
                    :components 
                    ((:module "stub"
                      :components 
                      ((:file "stub")))
                     (:module "cache"
                      :components 
                      ())))))  

                 (:file "dns"))))

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
