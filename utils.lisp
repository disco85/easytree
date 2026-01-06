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

(defun next-fname-start (st ev)
  (cond
    ((and (eql st :decorations) (eql ev :dash))
     (cons :dash nil))
    ((and (eql st :decorations) (eql ev :decorations))
     (cons :decorations nil))
    ((and (eql st :dash) (eql ev :space))
     (cons :space nil))
    ((and (eql st :dash) (eql ev :dash))
     (cons :dash nil))
    ((and (eql st :dash) (eql ev :alphanum))
     (cons :alphanum t))
    ((and (eql st :space) (eql ev :alphanum))
     (cons :alphanum t))
    (t (cons :unexpected nil))))
