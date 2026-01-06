(in-package "ACL2")

(include-book "utils")

(def-pop-until pop-until-equal-zero some-pop-until-equal-zero :criteria zerop)

(defthm pop-until-zero-never-expands
  (<= (len (pop-until-equal-zero stack include))
      (len stack)))

;; (defun some-zerop (lst)
;;   (cond
;;     ((endp lst) nil)
;;     ((zerop (car lst)) t)
;;     (t (some-zerop (cdr lst)))))

(defthm pop-until-zero-shrinks-if-elem-exists
  (implies (some-pop-until-equal-zero stack)  ;; if item is in the stack
           (< (len (pop-until-equal-zero stack t))
              (len stack))))
