;; (defun pop-until (stack target include)
;;   "Return the list after removing elements up to the first occurrence of TARGET.
;; If INCLUDE is t, the TARGET element is also removed."
;;   (cond
;;     ((endp stack) nil) ; empty list
;;     ((equal (car stack) target)
;;      (if include
;;          (cdr stack) ; skip the target too
;;          stack))     ; keep the target
;;     (t (pop-until (cdr stack) target include))))
(in-package "ACL2")

(defmacro def-pop-until (name &key test criteria)
  `(defun ,name (stack include)
     (cond
       ((endp stack) nil)
       ((,test (car stack) ,criteria)
        (if include (cdr stack) stack))
       (t (,name (cdr stack) include)))))

;; (def-pop-until pop-until-equal-zero :test eql :criteria 0)

;; (defthm pop-until-zero-1
;;   (<= (len (pop-until-equal-zero stack include))
;;       (len stack)))
