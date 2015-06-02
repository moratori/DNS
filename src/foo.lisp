

(ql:quickload :cl-annot)
(in-package :cl-user)
(defpackage :foo
  (:use :cl
        :cl-annot
        :cl-annot.class
        )
  )
(in-package :foo)

(enable-annot-syntax )


@export-structure
(defstruct foo
  (a 0)
  (b 1)
  )


