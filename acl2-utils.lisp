(in-package "ACL2")

(include-book "utils")

(def-pop-until pop-until-equal-zero :test eql :criteria 0)

(defthm pop-until-zero-1
  (<= (len (pop-until-equal-zero stack include))
      (len stack)))

(defthm pop-until-zero-decreases
  (implies (member 0 stack)  ;; if 0 is in the stack
           (< (len (pop-until-equal-zero stack t))
              (len stack))))
