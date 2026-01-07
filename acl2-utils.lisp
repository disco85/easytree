;; :set-checkpoint-summary-limit 2000

(in-package "ACL2")

(include-book "utils")

;; "pop-until-zero" is pop-until some item is zerop (predicate: equals to zero)
(def-pop-until pop-until-zero some-item-zerop :criteria zerop)

;; Pop never expands/increase size of a stack
;; (defthm pop-until-zero--never-expands
;;   (<= (len (pop-until-zero stack include))
;;       (len stack)))

(defthm not-equal-nil-not-found
    (not (equal nil :not-found))
  :rule-classes nil)

(defthm not-equal-cons-not-found
  (implies (consp x)
           (not (equal (cons a x) :not-found))))

(defthm cdr-of-cons-not-not-found
  (implies (true-listp stack)
           (not (equal (cdr stack) :not-found))))

;; If elem exists (see: some-item-zerop stack), the stack after pop shrinks
(defthm pop-until-zero--shrinks-if-elem-exists
    (implies (and (true-listp stack)
                  (some-item-zerop stack))
             (< (len (pop-until-zero stack t))
                (len stack)))
  :hints (("Goal" :use cdr-of-cons-not-not-found)))

;; (defthm pop-until-zero--same-size-if-no-elem
;;   (implies (not (some-item-zerop stack))
;;            (= (len (pop-until-zero stack t))
;;               (len stack))))

;; BUGGY:
;; (defthm pop-until-zero--same-size-if-no-elem
;;   (implies (not (some-item-zerop stack))
;;            (= (len (pop-until-zero stack stack t))
;;               0)))

;; Loops are in :decorations :dash only ("loops" mean don't stop, ie, ended-p is false and
;; stays in the same state, ie new-st = st):
;; (defthm next-fname--loops
;;     (implies (and (member-eq st '(:decorations :dash))
;;                   (eql st ev))
;;              (let* ((result (next-fname st ev))
;;                     (new-st (car result))
;;                     (ended-p (cdr result)))
;;                (and (eql new-st st)
;;                     (null ended-p)))))
