(defpackage :acl2
  (:use :cl)
  (:export :*fname-terms* :next-fname-event))

(defpackage :easytree
  (:use :cl)
  (:export :main))

(defpackage :easytree-tests
  (:use :cl :easytree))
