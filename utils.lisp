(in-package "ACL2")  ;; functions are provable in ACL2 and must be in this package to be recognized by ACL2

;; (defun make-symbol-like (base suffix)
;;   #+acl2
;;   (packn (list base suffix))
;;   #-acl2
;;   (intern (format nil "~A-~A" base suffix) (symbol-package base)))

(defmacro def-pop-until (name pred-name &key criteria)
  "Defines a POP-UNTIL function and a predicate for checking the stopping condition.
   Works in ACL2 (uses PACKN) and in plain Common Lisp (uses FORMAT/INTERN)."
  (let ((recur-name
          #+acl2
            (packn (list name "-RECUR"))
          #-acl2
            (intern (format nil "~A-RECUR" name) (symbol-package name))))
    `(progn
       ;; Recursive helper
       (defun ,recur-name (stack crit-init include)
         (cond
           ((endp stack) :not-found)
           ((,criteria crit-init (car stack))
            (if include (cdr stack) stack))
           (t (,recur-name (cdr stack) crit-init include))))
       ;; Top-level function
       (defun ,name (stack crit-init include)
         (let ((res (,recur-name stack crit-init include)))
           (if (eql res :not-found) stack res)))
       ;; Predicate function
       (defun ,pred-name (stack crit-init)
         (cond
           ((endp stack) nil)
           ((,criteria crit-init (car stack)) t)
           (t (,pred-name (cdr stack) crit-init)))))))


;; (defun until-el-p (crit-el el) (= crit-el el))
;; (def-pop-until pop-until-zero some-item-zerop :criteria until-el-p)

#+acl2
(defconst *fname-terms* '(:decorations :dash :space :fnamechars :unexpected))
#-acl2
(defparameter *fname-terms* '(:decorations :dash :space :fnamechars :unexpected))

;; #+acl2
;; (defconst *fname-valid-terms* '(:decorations :dash :space :fnamechars))
;; #-acl2
;; (defparameter *fname-valid-terms* '(:decorations :dash :space :fnamechars))
#+acl2
(defconst *fname-invalid-terms* '(:unexpected))
#-acl2
(defparameter *fname-invalid-terms* '(:unexpected))

;; *terminal-fname-terms* is good for ACL2 proof, but I keep it here, maybe for some future usage:
#+acl2
(defconst *terminal-fname-terms* '(:fnamechars :unexpected))
#-acl2
(defparameter *terminal-fname-terms* '(:fnamechars :unexpected))

#+acl2
(defconst *next-fname-events* '(:decorations :dash :space :fnamechars))
#-acl2
(defparameter *next-fname-events* '(:decorations :dash :space :fnamechars))

(defun next-fname (st ev)
  (cond
    ((and (eql st :decorations) (eql ev :dash))        (cons :dash nil))
    ((and (eql st :decorations) (eql ev :decorations)) (cons :decorations nil))
    ((and (eql st :decorations) (eql ev :space))       (cons :decorations nil))
    ((and (eql st :dash) (eql ev :space))              (cons :space nil))
    ((and (eql st :dash) (eql ev :dash))               (cons :dash nil))
    ((and (eql st :dash) (eql ev :fnamechars))         (cons :fnamechars t))
    ((and (eql st :space) (eql ev :fnamechars))        (cons :fnamechars t))
    (t                                                 (cons :unexpected nil))))

(defun next-fname-event (ch)
  (cond
    ((member ch (coerce "+`├" 'list)) :decorations)
    ((member ch (coerce "─-" 'list)) :dash)
    ((member ch (coerce " 	" 'list)) :space)
    ((not (member ch (coerce " 	" 'list))) :fnamechars)))

#+acl2
(defun split-string-at (str num)
  (declare (xargs :guard (and (stringp str)
                              (natp num)
                              (<= num (length str)))))
  (let* ((char-list (coerce str 'list))
         (first-part (take num char-list))
         (second-part (nthcdr num char-list)))
    (cons (coerce first-part 'string)
          (coerce second-part 'string))))
#-acl2
(declaim (ftype (function (string integer) cons) split-string-at))
#-acl2
(defun split-string-at (str num)
  "Signals a condition if NUM is out of bounds of STR, else returns (str-before-num . str-after-num)
treating NUM index in the same way as Python does it"
  (cons (subseq str 0 num) (subseq str num)))

;; Further we suppose the context stack (ctx-stack) contains elements like (cons indent fname),
;; where fname is the current file/dir name from the output of tree(1) as is!

(defun until-indent-p (crit-indent ctx-stack-el)
  (let ((ctx-indent (car ctx-stack-el)))
      (= ctx-indent crit-indent)))

(def-pop-until pop-until-same-indent some-pop-until-same-indent-p :criteria until-indent-p)

#+acl2
(defun make-ctx () (list nil nil))

#+acl2
(defun ctx-last-fname (ctx) (car ctx))

#+acl2
(defun ctx-stack (ctx) (cadr ctx))

#+acl2
(defun set-ctx-last-fname (ctx new-last-fname) (cons new-last-fname (cadr ctx)))
#-acl2
(defun set-ctx-last-fname (ctx new-last-fname) (setf (ctx-last-fname ctx) new-last-fname))

#+acl2
(defun set-ctx-stack (ctx new-stack) (cons (car ctx) (list new-stack)))
#-acl2
(defun set-ctx-stack (ctx new-stack) (setf (ctx-stack ctx) new-stack))

#-acl2
(defstruct ctx last-fname stack)

(defun push-indent-in-ctx-stack (ctx-stack indent fname)
  (cons (cons indent fname) ctx-stack))

(defun push-indent-in-ctx (ctx indent fname)
  (let ((new-stack (push-indent-in-ctx-stack (ctx-stack ctx) indent fname)))
    (set-ctx-stack ctx new-stack)))

(defun add-fname-to-path (path fname)
  "Adds fname to a path as: (fname path-comp2 path-comp1), ie to the head. Later such
path will be converted to a string"
  (cons fname path))

(defun extract-fname-and-indent-1 (ctx line line-num results) ;; TODO
  (if (= 1 line-num)
      (_)
      (_)))
