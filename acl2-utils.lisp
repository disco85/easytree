;; :set-checkpoint-summary-limit 2000

(in-package "ACL2")

;; (set-ld-skip-proofsp t state)
;; (include-book "centaur/fty/top" :dir :system) ;; :uncertified-okp t)
(include-book "std/lists/suffixp" :dir :system)
(include-book "std/lists/prefixp" :dir :system)
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
(defun next-fname-state--run (st evs)
  (if (endp evs)
      st
      (next-fname-state--run (next-fname-state st (car evs))
                             (cdr evs))))

;; Checks the codomain of next-fname - it's (cons <*fname-terms*> <booleanp>)
(defthm next-fname--codomain
    (implies (and (member-eq st acl2::*fname-terms*)
                  (member-eq ev acl2::*next-fname-events*))
             (and (consp (next-fname st ev))
                  (member-eq (car (next-fname st ev))
                             acl2::*fname-terms*)
                  (booleanp (cdr (next-fname st ev))))))

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
(defthm next-fname-state--ended-in-alphanum
  (iff (next-fname-ended-p st ev)
       (equal (next-fname-state st ev) :fnamechars)))

;; For any `ch` `(next-fname-event ch)` is T (not NIL), ie, the func is total:
(defthm next-fname-event--total
  (next-fname-event ch))

;; Similar to previous but checking particular codomain of `next-fname-event`:
(defthm next-fname-event--codomain
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
             (member-eq (next-fname-state--run st evs)
                        *fname-terms*)))

;; Invariant: if next-fname ended (it returns a flag of correct end!), it is always in fnamechars:
(defthm next-fname--run-ended-implies-fnamechars
    (implies (next-fname-ended-p (next-fname-state--run st evs)
                                 (car evs))
             (equal (next-fname-state--run st evs)
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




;;;;;;;;;;;;;;;;;;;;;;; --- extract-fname-and-indent-1
;; (defthm cdr--when-decreases
;;     (implies (true-listp xs) (<= (len (cdr xs)) (len xs))))

;; ;; (defthm cdr--when-zero
;; ;;     (implies (and (consp xs) (endp xs)) (= 0 (len (cdr xs)))))

;; (defthm subseq--when-decreases
;;     (implies (and (stringp s)
;;                   (<= m (len s))
;;                   (>= m n)
;;                   (natp m)
;;                   (natp n))
;;              (<= (len (subseq s n m)) (len s))))
;; (defthm subseq--when-equals
;;     (implies (and (stringp s)
;;                   (eql m (len s))
;;                   (eql n 0))
;;              (= (len (subseq s n m)) (len s))))
;; (defthm subseq--when-zero
;;     (implies (and (stringp s)
;;                   (= m n))
;;              (= (len (subseq s n m)) 0)))

;; (defthm split-string-at--when-decreases
;;     (implies (and (stringp s)
;;                   (< 0 (len s))
;;                   (natp n))
;;              (< (len (cdr (split-string-at s n)))
;;                 (len s))))

;; (defthm split-string-at--when-equals
;;     (implies (and (stringp s)
;;                   (< 0 (len s))
;;                   (= 0 n))
;;              (= (len (cdr (split-string-at s n)))
;;                 (len s))))
;; (defthm split-string-at--when-zero
;;     (implies (and (stringp s)
;;                   (< 0 (len s))
;;                   (= n (len s)))
;;              (= (len (cdr (split-string-at s n)))
;;                 0)))

;; =================================================================================================
;; extract-fname-and-indent-1 and extract-fname-and-indent verification
;; =================================================================================================

;; The codomain is (NIL, <some-string>)
(defthm extract-fname-and-indent--codomain
  (implies (stringp str)
           (or (null (extract-fname-and-indent str))
               (stringp (extract-fname-and-indent str)))))

;; FSM ends in :FNAMECHARS when FNAME and INDENT were extracted, ie (EXTRACT-FNAME-AND-INDENT str) returns not NIL
(defthm extract-fname-and-indent--ends-in-fnamechars-when-was-extracted
  (implies (and (stringp s)
                (extract-fname-and-indent s))
           (eql (car (extract-fname-and-indent-1 s 0 :decorations))
                :fnamechars)))


;; Checks detailed codomain of EXTRACT-FNAME-AND-INDENT-1: (CONS <*terminal-fname-terms*> <INTEGERP>)
(defthm extract-fname-and-indent-1--codomain
    (implies (and (stringp str)
                  (integerp idx))
             (let ((r (extract-fname-and-indent-1 str idx st)))
               (and (consp r)
                    (member-eq (car r) acl2::*terminal-fname-terms*)
                    (integerp (cdr r)))))
  :hints (("Goal" :induct (extract-fname-and-indent-1 str idx st))))

;; If (EXTRACT-FNAME-AND-INDENT STR) failed then FSM ends in :UNEXPECTED state:
(defthm extract-fname-and-indent--ends-in-unexpected-when-was-not-extracted
    (implies (and (stringp str)
                  (not (extract-fname-and-indent str)))
             (eql (car (extract-fname-and-indent-1 str 0 :decorations))
                  :unexpected)))

;; Reversed to the prev: if FSM ends in :UNEXPECTED, EXTRACT-FNAME-AND-INDENT failed:
(defthm extract-fname-and-indent--is-not-extracted-when-ends-in-unexpected
    (implies (and (stringp str)
                  (eql (car (extract-fname-and-indent-1 str 0 :decorations))
                       :unexpected))
             (not (extract-fname-and-indent str))))

;; Union of the both prev: united with IFF - it does not need the prev. 2 theorems:
(defthm extract-fname-and-indent--ends-in-unexpected-iff-was-not-extracted
    (implies (stringp str)
             (iff (not (extract-fname-and-indent str))
                  (eql (car (extract-fname-and-indent-1 str 0 :decorations))
                       :unexpected))))

(defun ctx-p (ctx)
  "Predicate checking that CTX is alist playing the role of CTX structure (the same name).
We avoid CTXP name bcs such symbol already exists in ACL2 theorem prover"
  (and (alistp ctx)
       (assoc-equal :indents ctx)
       (assoc-equal :fnames ctx)
       (assoc-equal :curdir ctx)
       (let ((indents (cdr (assoc-equal :indents ctx)))
             (fnames  (cdr (assoc-equal :fnames ctx)))
             (curdir  (cdr (assoc-equal :curdir ctx))))
         (and (or (null indents) (listp indents))
              (or (null fnames)  (listp fnames))
              (or (null curdir)  (stringp curdir))))))

;; Checks that PUSH-FNAME-AND-INDENT leads to the same result as PUSH-FNAME and then PUSH-FNAME-INDENT
(defthm push-fname-and-indent--steps
    (implies (and (ctx-p ctx)
                  (stringp fname)
                  (integerp indent))
             (let* ((ctx1 (push-fname-and-indent ctx fname indent))
                    (ctx2 (push-fname ctx fname))
                    (ctx3 (push-indent ctx2 indent)))
               (equal ctx1 ctx3))))

;; Checks the codomain of CMP-NEW-INDENT-WITH-OLD function
(defthm cmp-new-indent-with-old--codomain
    (implies (and (ctx-p ctx)
                  (integerp new-indent))
             (let ((res (cmp-new-indent-with-old ctx new-indent)))
               (member res '(:to-right :to-left :same)))))

;; Lemma: coerce a string to a list creates a TRUE-LISTP (see below):
(defthm coerce-string-to-true-list
    (implies (stringp str)
             (true-listp (coerce str 'list))))

;; Lemma: NIL is suffix of any TRUE-LISTP (see below):
(defthm suffixp-nil-true-listp
    (implies (true-listp x)
             (equal (suffixp nil x) t)))

;; Checks that the result of the lest trim is a suffix of the original string. It needs 2 lemmas
;; bcs it cannot work with strings easy, so we convert strings to lists with COERCE and then it
;; needs those lemmas: COERCE-STRING-TO-TRUE-LIST, SUFFIXP-NIL-TRUE-LISTP: 1st inserts into the
;; context theorem for proving that coercion of string to list is TRUE-LISTP (it's strange but
;; it is needed). The 2nd: requires proving that NIL is suffix of any TRUE-LISTP and we write
;; it as a rewrite rule: (equal lhs rhs):
;; ACL2 does not automatically know:
;;     - that a string has length
;;     - that coercing it to a list yields a proper list
;;     - that the result ends in nil
;;     - that the result is a consp unless the string is empty
;; All of these must be proven.
(defthm string-left-trim1--suffix
    (implies (and (stringp str)
                  (character-listp chars))
             (suffixp (coerce (string-left-trim1 chars str) 'list)
                      (coerce str 'list))))

;; Checks that left trim never extends a string:
(defthm string-left-trim1--never-extends
    (implies (and (stringp str)
                  (character-listp chars))
             (>= (length str) (length (string-left-trim1 chars str)))))

;; Checks that right trim keeps a prefix of a string:
(defthm string-right-trim1--suffix
    (implies (and (stringp str)
                  (character-listp chars))
             (prefixp (coerce (string-right-trim1 chars str) 'list)
                      (coerce str 'list))))

;; Checks that right trim never extends a string:
(defthm string-right-trim1--never-extends
    (implies (and (stringp str)
                  (character-listp chars))
             (>= (length str) (length (string-right-trim1 chars str)))))

;; Checks that after right trim, no trimmed chars in the end:
(defthm string-right-trim1--no-trailing-removable-chars
    (implies (and (stringp str)
                  (characterp ch))
             (not (equal (char (string-right-trim1 (list ch) str)
                               (1- (length (string-right-trim1 (list ch) str))))
                         ch))))

;; Checks that after left trim, no trimmed chars in the beginning:
(defthm string-left-trim1--no-leading-removable-chars
    (implies (and (stringp str)
                  (characterp ch))
             (not (equal (char (string-left-trim1 (list ch) str) 0)
                         ch))))

;;;; TODO these .... for  join-path-components--number-of-delims
(defun count-slashes (str)
  (declare (xargs :guard (stringp str)))
  (if (zerop (length str))
      0
      (count #\/ (coerce str 'list))))

(defthm count-slashes--on-empty-string
    (implies (and (stringp str)
                  (zerop (length str)))
             (equal (count-slashes str) 0)))
(defthm count-slashes--on-null
    (implies (null str)
             (equal (count-slashes str) 0)))

(defthm true-listp-of-coerce-string ;; TODO maybe to remove
    (implies (stringp s)
             (true-listp (coerce s 'list))))

(defthm len-coerce-string-positive ;; TODO maybe to remove
    (implies (and (stringp s)
                  (< 0 (length s)))
             (< 0 (len (coerce s 'list)))))

(defthm len-of-append
    (equal (len (append x y))
           (+ (len x) (len y))))

;; (defthm len-zero-implies-nil
;;   (implies (and (true-listp x)
;;                 (equal (len x) 0))
;;            (null x)))

;; (defthm append-nil1
;;     (implies (and (true-listp lst1)
;;                   (null lst2))
;;              (equal (append lst1 lst2) lst1)))
;; (defthm append-nil2
;;     (implies (and (true-listp lst2)
;;                   (null lst1))
;;              (equal (append lst1 lst2) lst2)))

;; (defthm append-when-second-len-zero
;;   (implies (equal (len y) 0)
;;            (equal (append x y) x)))

;; (defthm append-when-first-len-zero
;;   (implies (equal (len x) 0)
;;            (equal (append x y) y)))
(defthm count-listp-of-append
    (implies (and (true-listp x)
                  (true-listp y))
             (equal (count-listp c (append x y) (len (append x y)))
                    (+ (count-listp c x (len x))
                       (count-listp c y (len y)))))
  ;; :hints (("Goal" :induct (len x)))
  )

;; ":use" instructs ACL2 to instantiate and apply a theorem as a rewrite or lemma.
;;
;; ":hints" guides the prover to use some theorems to proof the current one.
;;
;; :instance tells ACL2 to take an existing theorem and to substitute specific
;; values for its variables - it literally substitutes C, X, Y variables in
;; COUNT-LISTP-OF-APPEND and the substitution produces the instantiated lemma:
;;   (equal (count-listp #\/
;;                       (append (coerce str1 'list)
;;                               (coerce str2 'list))
;;                       (len (append (coerce str1 'list)
;;                                    (coerce str2 'list))))
;;          (+ (count-listp #\/ (coerce str1 'list) (len (coerce str1 'list)))
;;             (count-listp #\/ (coerce str2 'list) (len (coerce str2 'list)))))
;;
;; It says: prove a general list lemma, then instantiate it for strings.
;;
;; COUNT-SLASHES--DISTRIBUTIVE is a law COUNT-SLASHES(STR1+STR2) = COUNT-SLASHES(STR1) + COUNT-SLASHES(STR2)
(defthm count-slashes--concatenate-distributive
    (implies (and (stringp str1)
                  (stringp str2))
             (equal (count-slashes (concatenate 'string str1 str2))
                    (+ (count-slashes str1)
                       (count-slashes str2))))
  :hints (("Goal"
           :use ((:instance count-listp-of-append
                            (c #\/)
                            (x (coerce str1 'list))
                            (y (coerce str2 'list)))))))

(defthm count-slashes--concatenate-one
  (implies (stringp str1)
           (equal (count-slashes (concatenate 'string str1))
                  (count-slashes str1))))

(defthm slash-in-empty-str
    (equal (count-slashes "") 0))

(defthm slash-in-str
    (equal (count-slashes "/") 1))

;; (defthm concatenate-3
;;     (implies (and (stringp str1)
;;                   (stringp str2)
;;                   (stringp str3))
;;              (equal (concatenate 'string str1 str2 str3)
;;                     (concatenate 'string
;;                                  (concatenate 'string str1 str2)
;;                                  str3))))
(defthm append-associative-3
  (equal (append a b c)
         (append (append a b) c)))

(defthm concatenate-to-append
  (implies (and (stringp s1)
                (stringp s2))
           (equal (concatenate 'string s1 s2)
                  (coerce (append (coerce s1 'list)
                                  (coerce s2 'list))
                          'string))))

(defthm concatenate-3
  (implies (and (stringp str1)
                (stringp str2)
                (stringp str3))
           (equal (concatenate 'string str1 str2 str3)
                  (concatenate 'string
                               (concatenate 'string str1 str2)
                               str3)))
  ;; :hints (("Goal"
  ;;          :expand ((concatenate 'string str1 str2 str3)
  ;;                   (concatenate 'string str1 str2)
  ;;                   (concatenate 'string
  ;;                                (concatenate 'string str1 str2)
  ;;                                str3))))
  )

(defthm nonempty-string-corresponds-to-nonempty-list
    (implies (and (stringp str)
                  (< 0 (length str)))
             (< 0 (len (coerce str 'list)))))

;; (defthm nonempty-list-corresponds-to-nonempty-str
;;     (implies (and (true-listp lst)
;;                   (consp lst)
;;                   (< 0 (len lst)))
;;              (< 0 (len (coerce lst 'string)))))

;; (defthm count-slashes--join-with-slash
;;     (implies (and (stringp str1)
;;                   (stringp str2))
;;              (<= 1 (count-slashes (concatenate 'string str1 "/" str2))))
;;   ;; :use ((:instance count-listp-of-append
;;   ;;                  (c #\/)
;;   ;;                  (x (coerce str1 'list))
;;   ;;                  (y (coerce str2 'list))))
;;   ;; :in-theory (disable count-listp-of-append)
;;   )

;; (defthm count-of-append
;;   (equal (count-slashes (append x y))
;;          (+ (count-slashes x)
;;             (count-slashes y)))
;;   :hints (("Goal" :induct (len x))))

(defthm count-slashes-of-concatenate
    (implies (and (stringp str1)
                  (stringp str2))
             (equal (count-slashes (concatenate 'string str1 str2))
                    (+ (count-slashes str1)
                       (count-slashes str2))))
  :hints (("Goal"
           :use ((:instance count-listp-of-append
                            (c #\/)
                            (x (coerce str1 'list))
                            (y (coerce str2 'list))))))
  :rule-classes (:rewrite))

;; (defthm length-join-path-components-positive
;;   (implies (and (consp lst)
;;                 (string-listp lst))
;;            (< 0 (length (join-path-components lst))))
;;   :rule-classes (:rewrite))

;; (defthm len-coerce-string
;;   (implies (stringp s)
;;            (equal (len (coerce s 'list))
;;                   (length s)))
;;   :rule-classes (:rewrite))

;; Checks the result with NIL arg:
(defthm join-path-components--on-nil
    (implies (null lst)
             (null (join-path-components lst))))

(defthm join-path-components--is-empty
    (implies (equal lst '(""))
             (equal (join-path-components lst) "")))

(defthm join-path-components--is-empty1
    (implies (and (true-listp lst)
                  (consp lst)
                  (equal (join-path-components lst) ""))
             (equal '("") lst))
  :rule-classes :forward-chaining)

;; Checks the codomain if arg is non-empty list of strings:
;; (defthm join-path-components-stringp-nonempty
;;   (implies (and (string-listp lst)
;;                 (consp lst))
;;            (stringp (join-path-components lst)))
;;   :rule-classes (:rewrite))


;; (defthm len-coerce-string-zero
;;     (implies (stringp s)
;;              (equal (equal (len (coerce s 'list)) 0)
;;                     (equal s ""))))
;; (defthm len-coerce-string-zero-empty
;;     (implies (and (stringp s)
;;                   (equal (len (coerce s 'list)) 0))
;;              (equal s ""))
;;   :rule-classes :forward-chaining)

(defthm len-of-single-empty-string-list
    (equal (len '("")) 1))

;; (defthm join-path-components-empty
;;   (implies (not (consp lst))
;;            (equal (join-path-components lst) nil))
;;   :rule-classes (:rewrite))

(defthm len-positive-true-listp-implies-consp
    (implies (and (true-listp x)
                  (< 0 (len x)))
             (consp x)))

(defun empty-strings-list-p (lst first-call-call)
  #+acl2
  (declare (xargs :guard (and (true-listp lst) (booleanp first-call-call))))
  (if (endp lst)
      (if first-call-call nil t)
      (if (and (stringp (car lst))
               (equal (car lst) ""))
          (empty-strings-list-p (cdr lst) nil)
          nil)))

(defun nonempty-strings-list-p (lst first-call-call)
  #+acl2
  (declare (xargs :guard (and (true-listp lst) (booleanp first-call-call))))
  (if (endp lst)
      (if first-call-call nil t)
      (if (and (stringp (car lst))
               (not (equal (car lst) "")))
          (nonempty-strings-list-p (cdr lst) nil)
          nil)))

(defthm join-path-components--all-empty
    (implies (and (true-listp lst)
                  (consp lst)
                  (empty-strings-list-p lst t)
                  (< 1 (length lst)))
             (equal (join-path-components lst) "/")))

(defthm join-path-components--stringp
    (implies (and (true-listp lst)
                  (consp lst)
                  (string-listp lst))
             (stringp (join-path-components lst))))

(defthm join-path-components--all-nonempty-is-stringp
    (implies (and (true-listp lst)
                  (consp lst)
                  (nonempty-strings-list-p lst t))
             (stringp (join-path-components lst))))

(defthm all-nonempty-strings--for-tail
    (implies (and (true-listp lst)
                  (consp lst)
                  (< 1 (len lst))
                  (nonempty-strings-list-p lst t))
             (nonempty-strings-list-p (cdr lst) t)))

;; (defthm concatenate--nonempty
;;     (implies (and (stringp str1)
;;                   (stringp str2)
;;                   (nonempty-strings-list-p (list str1 str2) t))
;;              (nonempty-strings-list-p (list (concatenate 'string str1 str2)) t)))

(defthm join-path-components--all-nonempty-non-empty-stringp
    (implies (and (true-listp lst)
                  (consp lst)
                  (< 1 (len lst))
                  (nonempty-strings-list-p lst t))
             (let ((res (join-path-components lst)))
               (and (stringp res)
                    (< 0 (length res))))))

(defthm join-path-components--min-one-slash
    (implies (and (true-listp lst)
                  (consp lst)
                  (< 1 (len lst))
                  (nonempty-strings-list-p lst t))
             (let ((res (join-path-components lst)))
               (< 0 (count-slashes res)))))


;; Base case of induction:
(defthm join-path-components-len-1
    (implies (and (true-listp lst)
                  (equal (len lst) 1))
             (equal (count-slashes (join-path-components lst))
                    (count-slashes (car lst)))))

(defthm nonempty-strings-list-p-implies
    (implies (and (nonempty-strings-list-p lst t)
                  (member-equal x lst))
             (and (stringp x)
                  (not (equal x "")))))

(defthm join-path-components--structurization-of-2
    (implies (and (true-listp lst)
                  (= 2 (len lst)))
             (equal (join-path-components lst)
                    (join-path-components (list (car lst) (cadr lst))))))

;; (defthm nonempty-strings-list-p-cdr
;;   (implies (and (nonempty-strings-list-p lst t)
;;                 (consp lst))
;;            (nonempty-strings-list-p (cdr lst) t)))

;; (defthm join-path-components-step
;;   (implies (and (consp lst)
;;                 (consp (cdr lst))
;;                 (nonempty-strings-list-p lst t))
;;            (>= (count-slashes (join-path-components lst))
;;                (+ 1 (count-slashes (join-path-components (cdr lst))))))
;;   :hints (("Goal"
;;            :expand ((join-path-components lst)))))

;; (defthm join-path-components--min-slashes-num
;;     (implies (and (true-listp lst)
;;                   (nonempty-strings-list-p lst t)
;;                   (consp lst))
;;              (>= (count-slashes (join-path-components lst))
;;                  (1- (len lst))))
;;   :hints (("Goal"
;;            :induct (join-path-components lst))))


;; (defthm join-path-components--min-slashes-num
;;   (implies (and (true-listp lst)
;;                 (nonempty-strings-list-p lst t))
;;            (>= (count-slashes (join-path-components lst))
;;                (if (consp lst)
;;                    (1- (len lst))
;;                    0))))

;; (defthm count-slashes-of-join-path-components
;;   (implies (and (true-listp lst)
;;                 (nonempty-strings-list-p lst t))
;;            (>= (count-slashes (join-path-components lst))
;;                (- (len lst) 1))))

;; (defthm join-path-components--number-of-delims
;;     (implies (and (true-listp lst)
;;                   (string-listp lst))
;;              (let* ((n (len lst))
;;                     (min-delims (if (> n 0) (1- n) 0)))
;;                (<= min-delims (count-slashes (join-path-components lst)))))
;;   ;; :hints (("Goal" :induct (len lst)))
;;   ;; :hints (("Goal" :use count-slashes-of-concatenate))
;;   )

;; Now we have big functions using small helper-functions with 2 different implementations:
;; 1) #+acl2 (for the world of logic: list-based) and 2) #-acl2 (for the world of CL: string-based)
;;
;; It's possible to keep everything in string and ACL2 will permanently convert it to list
;; during its proving but it can lead to problems: hanging, induction problems that are
;; not so easy for solving!
;;
;; Now we can prove it in ACL2 REPL as:
;;   ACL2 !>(extract-fname-and-indent '(#\  #\+ #\- #\- #\  #\a #\b))
;;   (#\a #\b)
;; or using a string and this wrapper:
;;   ACL2 !>(extract-fname-and-indent-wrapper " +-- ab")
;;   "ab"
;; Also we can prove that the result is expected using logic (not exec!) version,
;; ACL2 will use logic version, so the list-based recursion, #+acl2 helpers:
;;   ACL2 !>(thm (equal (extract-fname-and-indent-wrapper " +-- ab") "ab"))
;;   Q.E.D.
;; But ACL2 cannot check guards for exec version, so we turn them off (:verify-guards nill) -
;; the rule is: IF YOU WANT ACL2 TO VERIFY GUARDS FOR THE EXEC VERSION,
;; THE LOGIC AND EXEC VERSIONS MUST USE THE SAME DATA REPRESENTATION. Bcs
;; ACL2 sees only #+acl2 branches (with list-based version). It cannot see
;; #-acl2 at all, on the read code phase!
;; So, the scenarios are:
;;   1) logic, exec - use list based code -- guards of exec will be verified (which means
;;      that ACL2 image can run them safely, like refined types check before compilation).
;;      Guards for logic are not so important: we have checks in (implies ...) for theorems.
;;   2) logic, exec - use string based code -- but theorem proving will be difficult for
;;      induction on strings (even when ACL2 convert to/from behind the scene)
;;   3) or hybrid code - like extract-fname-and-indent, but then turn-off guards check,
;;      bcs guards available to ACL2 compilation phase are under #+acl2 and they reflect
;;      only logic version!
(defun extract-fname-and-indent-wrapper (str)
  (declare (xargs :guard (stringp str) :verify-guards nil))
  (mbe
    :logic (coerce (extract-fname-and-indent (coerce str 'list)) 'string) ;; FIXME now it returns (str . pos)
    :exec  (extract-fname-and-indent str)))
