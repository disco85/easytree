(in-package "ACL2")

;; (defun make-symbol-like (base suffix)
;;   #+acl2
;;   (packn (list base suffix))
;;   #-acl2
;;   (intern (format nil "~A-~A" base suffix) (symbol-package base)))

(defmacro def-pop-until (name pred-name &key criteria)
  "Defines a POP-UNTIL function and a predicate for checking the stopping condition.
   Works in ACL2 (uses PACKN) and in plain Common Lisp (uses FORMAT/INTERN)."
  (let ((rec-name
          #+acl2
            (packn (list name "-REC"))
          #-acl2
            (intern (format nil "~A-REC" name) (symbol-package name))))
    `(progn
       ;; Recursive helper
       (defun ,rec-name (stack include)
         (cond
           ((endp stack) :not-found)
           ((,criteria (car stack))
            (if include (cdr stack) stack))
           (t (,rec-name (cdr stack) include))))
       ;; Top-level function
       (defun ,name (stack include)
         (let ((res (,rec-name stack include)))
           (if (eql res :not-found) stack res)))
       ;; Predicate function
       (defun ,pred-name (stack)
         (cond
           ((endp stack) nil)
           ((,criteria (car stack)) t)
           (t (,pred-name (cdr stack))))))))


;; (defmacro def-pop-until (name pred-name &key criteria)
;;   (let ((rec-name (make-symbol-like name 'rec)))
;;     `(progn
;;      (defun ,rec-name (orig-stack stack include)
;;        (cond
;;          ((endp stack) orig-stack)
;;          ((,criteria (car stack))
;;           (if include (cdr stack) stack))
;;          (t (,name orig-stack (cdr stack) include))))
;;      (defun ,name (stack include) (,rec-name stack stack include))
;;      (defun ,pred-name (stack)
;;        (cond
;;          ((endp stack) nil)
;;          ((,criteria (car stack)) t)
;;          (t (,pred-name (cdr stack))))))))

(def-pop-until pop-until-equal-zero some-pop-until-equal-zero :criteria zerop)

#+acl2
(defconst *fname-terms* '(:decorations :dash :space :fnamechars))
#-acl2
(defparameter *fname-terms* '(:decorations :dash :space :fnamechars))

(defun next-fname (st ev)
  (cond
    ((and (eql st :decorations) (eql ev :dash))
     (cons :dash nil))
    ((and (eql st :decorations) (eql ev :decorations))
     (cons :decorations nil))
    ((and (eql st :decorations) (eql ev :space))
     (cons :decorations nil))

    ((and (eql st :dash) (eql ev :space))
     (cons :space nil))
    ((and (eql st :dash) (eql ev :dash))
     (cons :dash nil))
    ((and (eql st :dash) (eql ev :fnamechars))
     (cons :fnamechars t))

    ((and (eql st :space) (eql ev :fnamechars))
     (cons :fnamechars t))

    (t (cons :unexpected nil))))


(defun next-fname-event (ch)
  (cond
    ((member ch (coerce "+`├" 'list)) :decorations)
    ((member ch (coerce "─-" 'list)) :dash)
    ((member ch (coerce " 	" 'list)) :space)
    ((not (member ch (coerce " 	" 'list))) :fnamechars)))
