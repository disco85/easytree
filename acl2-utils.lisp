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

(defthm pop-until-zero-rec--not-found-if-no-elem
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

;; Short alias - it calls FSM next step but ACL2 just uses it for rewrite:
(defun next-fname-state (st ev)
  (car (next-fname st ev)))

;; Short alias - it calls FSM next step but ACL2 just uses it for rewrite:
(defun next-fname-ended-p (st ev)
  (cdr (next-fname st ev)))

;; Loops are in :decorations :dash only ("loops" mean don't stop, ie, ended-p is false and
;; stays in the same state, ie new-st = st):
(defthm next-fname--loops
    (implies (and (member-eq st '(:decorations :dash))
                  (eql st ev))
             (and (eql (next-fname-state st ev) st)
                  (not (next-fname-ended-p st ev)))))

;; FSM ends in :fnamechars state only:
;; XXX If you repeat `(next-fname st ev)`, it's OK, ACL2 does not call next-fname, and if
;;     textually/syntactically they are the same - logically it's the same term. You see
;;     it in `next-fname-state` and `next-fname-ended-p` above
(defthm next-fname--ended-in-alphanum
  (iff (next-fname-ended-p st ev)
       (equal (next-fname-state st ev) :fnamechars)))

;; For any `ch` `(next-fname-event ch)` is T (not NIL), ie, the func is total:
(defthm next-fname-event-total
  (next-fname-event ch))

;; Similar to previous but checking particular domain of `next-fname-event`:
(defthm next-fname-event--domain
  (member (next-fname-event ch) *fname-terms*))






;;---------------------------



(defconst *events* '(:decorations :dash :space :fnamechars))

(defthm events-are-true-list
    (true-listp *events*))

(defun can-leave-p (st evs)
  ;; XXX `:measure (len evs)` gives a metrics of a function progress: if it does not change -
  ;;     it is treated as live-lock (inf. loop), it allows to detect inf. recursion of can-leave-p.
  ;; XXX `:guard (true-listp evs)` says that `evs` is not cons but proper list, it means that:
  ;;     `(len (cdr evs)) <= (len evs)`, it helps ACL2 to change the measure/metrics on each step.
  ;;     Also it helps to prevent ACL2 from checking when `evs` is string, assoc, etc (it does it).
  ;; XXX It's possible to work without this `(declare ...)` and `events-are-true-list` at all!
  ;; XXX `:verify-guards nil` tells ACL2 not to satisfy guards of funcs called inside `can-leave-p`.
  ;;     If to use this `(declare ...)` at all - it's needed, else - fails: `next-fname` has implicit
  ;;     guards and it's pain for ACL2 to prove them, they are:
  ;;     - `evs` is true list
  ;;     - elements of `*events*` are symbols:
  ;;         (defthm events-elements-are-symbols
  ;;             (implies (member-eq ev *events*)
  ;;                      (symbolp ev)))
  ;;     - `next-fname` always returns cons:
  ;;         (defthm next-fname-returns-cons
  ;;           (implies (and (symbolp st) (symbolp ev))
  ;;                    (consp (next-fname st ev))))
  ;;     - car of `next-fname` result is a symbol:
  ;;         (defthm next-fname-car-is-symbol
  ;;           (implies (and (symbolp st) (symbolp ev))
  ;;                    (symbolp (car (next-fname st ev)))))
  ;;     - optionally list's length lemma:
  ;;         (defthm len-cdr-less
  ;;           (implies (consp evs)
  ;;                    (< (len (cdr evs)) (len evs))))
  ;;
  (declare (xargs :measure (len evs) :guard (true-listp evs) :verify-guards nil))
  (if (endp evs)
      nil
    (or (not (equal (car (next-fname st (car evs))) st))
        (can-leave-p st (cdr evs)))))

(defthm non-terminal-states-can-leave
  (implies (and (member-eq st *fname-terms*)
                (not (member-eq st *terminal-fname-terms*)))
           (can-leave-p st *events*)))
