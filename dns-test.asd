#|
  This file is a part of dns project.
  Copyright (c) 2015 moratori (teldev@live.jp)
|#

(in-package :cl-user)
(defpackage dns-test-asd
  (:use :cl :asdf))
(in-package :dns-test-asd)

(defsystem dns-test
  :author "moratori"
  :license "LLGPL"
  :depends-on (:dns
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "dns"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
