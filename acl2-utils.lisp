;; :set-checkpoint-summary-limit 2000

(in-package "ACL2")

(include-book "utils")

;; "pop-until-zero" is pop-until some item is zerop (predicate: equals to zero)
(def-pop-until pop-until-zero some-item-zerop :criteria zerop)

;; Pop never expands/increase size of a stack
(defthm pop-until-zero--never-expands
  (<= (len (pop-until-zero stack include))
      (len stack)))

;; Lemma: NIL is not :not-found. Coz it's obvious, ACL2 will create
;; rewrite rule from it: X1 -> X1, where X1 ("NIL is not :not-found")
;; is trivial T (true), so it will create rewrite rule like T -> ...
;; And then ACL2 will not use such rewrite rule bcs it's tautology (no
;; reason to rewrite T to something else, moreover to T). We add
;; ":rule-classes nil" to prevent X1 to convert it to rewrite rule, so
;; later it can be used in ":use ..." or ":in-theory ..."
;; (defthm not-equal-nil-not-found
;;     (not (equal nil :not-found))
;;   :rule-classes nil)

;; (defthm not-equal-cons-not-found
;;   (implies (consp x)
;;            (not (equal (cons a x) :not-found))))

;; (defthm cdr-of-cons-not-not-found
;;   (implies (true-listp stack)
;;            (not (equal (cdr stack) :not-found))))

;; If elem exists (see: some-item-zerop stack), the stack after pop shrinks
;; (defthm pop-until-zero--shrinks-if-elem-exists
;;     (implies (and (true-listp stack)
;;                   (some-item-zerop stack))
;;              (< (len (pop-until-zero stack t))
;;                 (len stack)))
;;   ;;:hints (("Goal" :use cdr-of-cons-not-not-found))
;;   )

(defthm pop-until-zero-rec-not-found-if-no-zero
  (implies (and (true-listp stack)
                (not (some-item-zerop stack)))
           (equal (pop-until-zero-rec stack t)
                  :not-found)))

(defthm pop-until-zero--same-size-if-no-elem
    (implies (and (true-listp stack)
                  (not (some-item-zerop stack)))
             (= (len (pop-until-zero stack t))
                (len stack))))

;; BUGGY:
;; (defthm pop-until-zero--same-size-if-no-elem
;;   (implies (not (some-item-zerop stack))
;;            (= (len (pop-until-zero stack stack t))
;;               0)))

;; Loops are in :decorations :dash only ("loops" mean don't stop, ie, ended-p is false and
;; stays in the same state, ie new-st = st):
(defthm next-fname--loops
    (implies (and (member-eq st '(:decorations :dash))
                  (eql st ev))
             (let* ((result (next-fname st ev))
                    (new-st (car result))
                    (ended-p (cdr result)))
               (and (eql new-st st)
                    (null ended-p)))))
