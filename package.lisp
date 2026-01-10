(defpackage :acl2
  (:use :cl)
  (:export
   :next-fname
   :next-fname-event
   :*fname-terms*
   *terminal-fname-terms*
   *next-fname-events*))

(defpackage :easytree
  (:use :cl)
  (:export :main))

(defpackage :easytree-tests
  (:use :cl :easytree)
  (:shadowing-import-from :check-it :tuple :generate)
  (:import-from
   :check-it
   :check-it
   :generator
   :integer
   :def-generator
   :def-genex-macro
   ))
