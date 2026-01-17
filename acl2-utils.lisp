;; :set-checkpoint-summary-limit 2000

(in-package "ACL2")

(include-book "utils")

;; =================================================================================================
;; pop-until verification
;; =================================================================================================

(defun until-el-p (crit-el el)
  (= crit-el el))

;; "pop-until-zero" is pop-until some item is zerop (predicate: equals to zero)
(def-pop-until pop-until-zero some-item-zerop :criteria until-el-p)

;; Pop never expands/increase size of a stack
(defthm pop-until-zero--never-expands
  (<= (len (pop-until-zero stack 0 include))
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
;;                   (some-item-zerop stack 0))
;;              (< (len (pop-until-zero stack 0 t))
;;                 (len stack)))
;;   ;;:hints (("Goal" :use cdr-of-cons-not-not-found))
;;   )

(defthm pop-until-zero-recur--not-found-if-no-elem
  (implies (and (true-listp stack)
                (not (some-item-zerop stack 0)))
           (equal (pop-until-zero-recur stack 0 t)
                  :not-found)))

(defthm pop-until-zero--same-size-if-no-elem
    (implies (and (true-listp stack)
                  (not (some-item-zerop stack 0)))
             (= (len (pop-until-zero stack 0 t))
                (len stack))))

;; BUGGY:
;; (defthm pop-until-zero--same-size-if-no-elem
;;   (implies (not (some-item-zerop stack))
;;            (= (len (pop-until-zero stack stack t))
;;               0)))

;; =================================================================================================
;; next-fname verification
;; =================================================================================================

;; Short alias - it calls FSM next step but ACL2 just uses it for rewrite:
(defun next-fname-state (st ev)
  (car (next-fname st ev)))

;; Short alias - it calls FSM next step but ACL2 just uses it for rewrite:
(defun next-fname-ended-p (st ev)
  (cdr (next-fname st ev)))

;; Runs FSM from state ST event by event (from `EVS`) end returns the last state, ie
;; answers to "from state ST after run all events - what state do we end up in?"
(defun next-fname-run (st evs)
  (if (endp evs)
      st
    (next-fname-run (next-fname-state st (car evs))
                    (cdr evs))))

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

(defthm next-fname-events--are-true-list
    (true-listp *next-fname-events*))

(defun next-fname--state-can-be-left-on-some-of-events (st evs)
  "Predicate that state ST can be left on some event from a list of events EVS"
  ;; XXX `:measure (len evs)` gives a metrics of a function progress: if it does not change -
  ;;     it is treated as live-lock (inf. loop), it allows to detect inf. recursion of
  ;;     `next-fname--state-can-be-left-on-some-of-events`.
  ;; XXX `:guard (true-listp evs)` says that `evs` is not cons but proper list, it means that:
  ;;     `(len (cdr evs)) <= (len evs)`, it helps ACL2 to change the measure/metrics on each step.
  ;;     Also it helps to prevent ACL2 from checking when `evs` is string, assoc, etc (it does it).
  ;; XXX It's possible to work without this `(declare ...)` and `events-are-true-list` at all!
  ;; XXX `:verify-guards nil` tells ACL2 not to satisfy guards of funcs called inside
  ;;     `next-fname--state-can-be-left-on-some-of-events`.
  ;;     If to use this `(declare ...)` at all - it's needed, else - fails: `next-fname` has implicit
  ;;     guards and it's pain for ACL2 to prove them, they are:
  ;;     - `evs` is true list
  ;;     - elements of `*next-fname-events*` are symbols:
  ;;         (defthm events-elements-are-symbols
  ;;             (implies (member-eq ev *next-fname-events*)
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
      ;; No, the state cannot be left w/o events:
      nil
      ;; or the first event (`car evs`) was enough: `next-fname`
      ;; leaves the current state (it is CHANGED on `car evs`):
      (or (not (equal (next-fname-state st (car evs)) st))
          ;; or it happens somewhere further down the list of events:
          (next-fname--state-can-be-left-on-some-of-events st (cdr evs)))))

;; All states of `next-fname` FSM can be left if they are not terminal or more
;; accurate: FOR EACH NON-TERMINAL STATE, THERE EXISTS AN EVENT THAT CHANGES THE STATE,
;; or the non-terminal state can be left on some event:
(defthm next-fname--non-terminal-states-can-be-left
    (implies (and (member-eq st *fname-terms*)
                  (not (member-eq st *terminal-fname-terms*)))
             (next-fname--state-can-be-left-on-some-of-events st *next-fname-events*)))

(defun next-fname--state-has-valid-exit (st evs)
  "Helper recursive function: switch to another state on event from `evs`: event by event and check
that for every event from `evs` the swith is not loop and the next state is valid one"
  ;; XXX to provoke failure: comment in `next-fname` FSM path from some state to valid state
  (if (endp evs)
      nil
      (or (let ((next (next-fname-state st (car evs))))
            ;; `next` state is not the same as `st`, ie it is not a loop:
            (and (not (equal next st))
                 ;; next state is valid state":
                 (not (member-eq next *fname-invalid-terms*))))
          (next-fname--state-has-valid-exit st (cdr evs)))))

(defthm next-fname--non-terminal-states-have-valid-exit
    (implies (and (member-eq st *fname-terms*)
                  (not (member-eq st *terminal-fname-terms*)))
             (next-fname--state-has-valid-exit st *next-fname-events*)))

;; Reachability: :fnamechars is reachable from :decorations (there is exit from the labyrinth):
(defthm next-fname--reach-fnamechars-from-decorations
    (next-fname--state-has-valid-exit :decorations *next-fname-events*))

;; General reachability: FSM next-fname never leaves known states/terms of FSM (domain of func):
(defthm next-fname--reachable-states-are-known
    (implies (and (member-eq st *fname-terms*)
                  (true-listp evs))
             (member-eq (next-fname-run st evs)
                        *fname-terms*)))

;; Invariant: if next-fname ended (it returns a flag of correct end!), it is always in fnamechars:
(defthm next-fname--run-ended-implies-fnamechars
    (implies (next-fname-ended-p (next-fname-run st evs)
                                 (car evs))
             (equal (next-fname-run st evs)
                    :fnamechars)))

;; Invariant: once NEXT-FNAME leave :decorations, you never return to it.
(defthm next-fname--never-return-to-decorations
  (implies (and (equal st :decorations)
                (not (equal (next-fname-state st ev) :decorations)))
           (not (equal (next-fname-state (next-fname-state st ev) ev2)
                       :decorations))))

;; Determinism: NEXT-FNAME always switch to the same:
(defthm next-fname-unique-next-state
  (implies (and (equal (next-fname st ev) x)
                (equal (next-fname st ev) y))
           (equal x y))
  ;; XXX without this ACL2 fails with:
  ;;       A :REWRITE rule generated from NEXT-FNAME--DETERMINISTIC is illegal
  ;;       because it rewrites the term (NEXT-FNAME ST EV) to itself!
  ;;     Bcs ACL2 refuses rewrite rules of the form:
  ;;       (next-fname st ev) -> (next-fname st ev)
  ;;     so we disable rewrite rule: this proves determinism without creating a rewrite rule.
  ;; XXX Why it happens? Bcs ACL2 tries to turn any theorem into a rewrite rule unless
  ;;     you tell it not to, as:
  ;;       (lhs) -> (rhs)
  ;;     ACL2 sees:
  ;;       (implies (and (equal A x)
  ;;                     (equal A y))
  ;;                (equal x y))
  ;;     And without to detect tautologies, brutally it follows the algorithm:
  ;;       x = y. lhs will be x. rhs will be y. x is A. y is A. So lhs=A, rhs=A, so
  ;;       (lhs) -> (rhs) => A -> A.    <--- this leads to the loop!
  ;;     ACL2 tries to extract such a rule from the theorem. After macroexpansion ACL2 tries to
  ;;     extract:  (equal (next-fname st ev) x) -> (equal (next-fname st ev) y)  bcs call results
  ;;     are the same. And it ends with:
  ;;       (next-fname st ev) -> (next-fname st ev)
  ;;     But such rule is endless/loop, ie illegal, so - failure! We disable it.
  ;; XXX EVERY DEFTHM BECOMES A REWRITE RULE BY DEFAULT. ACL2 IS A TERM-REWRITING THEOREM PROVER.
  :rule-classes nil)

;; Totality of transition function of NEXT-FNAME (before we proved the totality of event function):
(defthm next-fname--total
  (implies (and (member-eq st *fname-terms*)
                (member-eq ev *next-fname-events*))
           ;; totality, ie the result is CONSP, ie, never NIL:
           (consp (next-fname st ev))))

;; More general totality of NEXT-FNAME:
;; - every valid state + event pair has a next state
;; - the next state is always valid
;; - the FSM is closed under transitions
(defthm next-fname--state-total
  (implies (and (member-eq st *fname-terms*)
                (member-eq ev *next-fname-events*))
           (member-eq (next-fname-state st ev)
                      *fname-terms*)))
