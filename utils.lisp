(in-package "ACL2")

(defmacro def-pop-until (name pred-name &key criteria)
  `(progn
     (defun ,name (stack include)
       (cond
         ((endp stack) nil)
         ((,criteria (car stack))
          (if include (cdr stack) stack))
         (t (,name (cdr stack) include))))
     (defun ,pred-name (stack)
       (cond
         ((endp stack) nil)
         ((,criteria (car stack)) t)
         (t (,pred-name (cdr stack)))))))

;; (def-pop-until pop-until-equal-zero some-pop-until-equal-zero :criteria zerop)
