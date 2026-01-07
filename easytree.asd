(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :fiveam)
    ;; prefer fiveam-asdf (ASDF integration) then fall back to fiveam
    (or (ignore-errors (asdf:load-system :fiveam-asdf))
        (ignore-errors (asdf:load-system :fiveam)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :check-it)
    (ignore-errors (asdf:load-system :check-it))))


(asdf:defsystem #:easytree
  :description "Reconstruct directory tree"
  :author "John Doe"
  :license  "GPL-3.0-or-later"
  :version "0.0.1"
  :serial t
  :depends-on (:clingon)
  :components ((:file "package")
               (:file "utils")
               (:file "easytree"))
  :build-operation "program-op"
  :build-pathname "easytree"
  :entry-point "easytree::main"
  :in-order-to ((test-op (test-op "easytree/test"))))

(asdf:defsystem #:easytree/test
  :depends-on (:easytree :fiveam :check-it)
  :components ((:file "easytree-test"))
  :perform (test-op (o c) (symbol-call :fiveam :run! :suite1)))
