(in-package :easytree-tests)

(5am:def-suite :suite1 :description "EasyTree Test Suite")
(5am:in-suite :suite1)

(5am:test
 pop-util-equal--1
 (5am:is (equal '((1) (2))
                '((1) (2)))))


(defparameter *non-terminal-states*
  '(:decorations :dash :space))
(defparameter *valid-next-states*
  '(:decorations :dash :space :fnamechars :unexpected))

;; Property:
;; For any non-terminal state and any event, next-fname must not
;; return the same non-terminal state (i.e. cannot get stuck).

;; (ql:quickload '(:fiveam :check-it))
;; dists/check-it/software
;; (load ".qlot/dists/check-it/software/check-it-20150709-git/src/package.lisp")
;; (load ".qlot/dists/check-it/software/check-it-20150709-git/src/util.lisp")
;; (load ".qlot/dists/check-it/software/check-it-20150709-git/src/generators.lisp")
;; (load ".qlot/dists/check-it/software/check-it-20150709-git/src/regenerate.lisp")
;; (load ".qlot/dists/check-it/software/check-it-20150709-git/src/shrink.lisp")
;; (load ".qlot/dists/check-it/software/check-it-20150709-git/src/check-it.lisp")

(defparameter *terminal-states* '(:fnamechars :unexpected))
(defparameter *valid-next-states* '(:decorations :dash :space :fnamechars :unexpected))

;; (def-genex-macro one-of (items)
;;   `(or ,@items))

;; (import 'easytree-tests::one-of :check-it)

;; (defparameter *non-terminal-fname-terms*
;;   (set-difference acl2:*fname-terms* acl2:*terminal-fname-terms* :test #'eql))
;(use-package :check-it)
;; (defparameter *xy-generator*
;;   (generator
;;     (tuple
;;       (or :x1 :x2 :x3)
;;       (or :y1 :y2))))
;; (shadowing-import '(check-it:tuple))
;; (import '(check-it:generator check-it:tuple check-it:def-generator))
;; (defmacro check-it-exists ((var generator) &body body)
;;   "Runs a property test over generated values and succeeds if the property holds for at least one value.
;; Example:
;; (it-exists (x (integer 0 100))
;;   (and (evenp x) (> x 90)))
;; "
;;   (let ((found (gensym "FOUND")))
;;     `(let ((,found nil))
;;        (check-it (,var ,generator)
;;                  (lambda (,var)
;;                    (when (progn ,@body)
;;                      (setf ,found t))
;;                    t)) ;; Always return true to continue testing
;;        (if ,found
;;            (format t "✓ Existential property held for at least one value.~%")
;;            (error "✗ Existential property did NOT hold for any value.")))))

;; (defmacro it-exists ((var generator) &body body)
;;   "Checks if there exists at least one generated value for which the property holds.
;;    Reports the first satisfying example if found."
;;   (let ((found (gensym "FOUND"))
;;         (example (gensym "EXAMPLE")))
;;     `(let ((,found nil)
;;            (,example nil))
;;        (check-it (,var ,generator)
;;          (lambda (,var)
;;            (when (progn ,@body)
;;              (unless ,found
;;                (setf ,found t
;;                      ,example ,var)))
;;            t))
;;        (fiveam:is-true ,found
;;          (format nil "Expected at least one value to satisfy the property, but none did."))
;;        (when ,found
;;          (format t "✓ Existential property held for ~A~%" ,example)))))

;; (defmacro it-exists ((var generator) &body body)
;;   "Return T if at least one generated value satisfies BODY, else NIL."
;;   (let ((found (gensym "FOUND"))
;;         (example (gensym "EXAMPLE")))
;;     `(let ((,found nil)
;;            (,example nil))
;;        (check-it (,var ,generator)
;;          (lambda (,var)
;;            (when (progn ,@body)
;;              (setf ,found t
;;                    ,example ,var))
;;            t))
;;        (when ,found
;;          (format t "✓ Existential property held for ~A~%" ,example))
;;        ,found)))
;; (defmacro it-exists ((var generator) &body body)
;;   (let ((found (gensym "FOUND"))
;;         (example (gensym "EXAMPLE")))
;;     `(let ((,found nil)
;;            (,example nil))
;;        (check-it (,var ,generator)
;;          (lambda (,var)
;;            (when (progn ,@body)
;;              (setf ,found t
;;                    ,example ,var))
;;            t))
;;        ,found)))

;; (defmacro it-exists ((var generator) &body body)
;;   (let ((found   (gensym "FOUND"))
;;         (example (gensym "EXAMPLE")))
;;     `(let ((,found nil)
;;            (,example nil))
;;        (check-it (,var ,generator)
;;          (lambda (,var)
;;            (when (progn ,@body)
;;              (setf ,found t
;;                    ,example ,var))
;;            t))
;;        ,found)))

;; (defmacro it-exists ((var generator) &body body)
;;   "Returns T if at least one generated value satisfies BODY, else NIL.
;;    Use inside a function or lambda to avoid premature macroexpansion."
;;   (let ((found (gensym "FOUND"))
;;         (example (gensym "EXAMPLE")))
;;     `(let ((,found nil)
;;            (,example nil))
;;        (check-it (,var ,generator)
;;          (lambda (,var)
;;            (when (progn ,@body)
;;              (setf ,found t
;;                    ,example ,var))
;;            t))
;;        ,found)))

;; (fiveam:def-assertion exists (value &optional message)
;;   (fiveam:fail-unless value
;;     (or message
;;         "Expected at least one generated value to satisfy the property.")))

;; (def-generator non-terminal-fname-terms-gen () (generator (check-it::or :decorations :dash :space)))
(def-generator non-terminal-fname-terms-gen () (eval `(generator (check-it::or ,@acl2::*fname-terms*))))

;; (def-generator fname-events-gen () (generator (check-it::or :decorations :dash :space :fnamechars)))
(def-generator fname-events-gen () (eval `(generator (check-it::or ,@acl2::*next-fname-events*))))
(defparameter fname-non-terminal-transitions-gen
  (generator (tuple (non-terminal-fname-terms-gen) (fname-events-gen))))

(5am:test
    next-fname--non-terminal-states-have-valid-exit
  (5am:is
   (every (lambda (old-st)
            (some (lambda (ev)
                    (let* ((new-st (car (acl2::next-fname old-st ev))))
                      (and (not (eql new-st :unexpected))
                           (not (eql new-st old-st)))))
                  acl2::*next-fname-events*))
          (set-difference acl2::*fname-terms* acl2::*terminal-fname-terms* :test #'eql))))

(5am:test
    next-fname--reach-fnamechars-from-decorations
  ;; prop(st_i) := ∃ ev_i ∈ *next-fname-events*, next-fname(:decoration, ev_i) = :fnamechars ∨ prop(next_fname(st_i))
  (labels ((prop (st_i)
             (some (lambda (ev_i)
                     (let ((st_i+1 (car (acl2::next-fname st_i ev_i))))
                       (and (not (eql st_i st_i+1))
                            (or (eql st_i+1 :fnamechars)
                                (prop st_i+1)))))
                   acl2::*next-fname-events*)))
    (5am:is (prop :decorations))))

;; (5am:test next-fname-1
;;   (5am:is (it-exists trans fname-non-terminal-transitions-gen
;;             (let* ((from-state (car trans))
;;                    (on-event (cadr trans))
;;                    (res (acl2::next-fname from-state on-event))
;;                    (new-state (car res)))
;;               (format t "!!!!!!!!!!!!!!!!!!!! ~A -> ~A~%" trans new-state)
;;               (and (not (eql new-state :unexpected))
;;                    (not (eql new-state from-state)))))))

;; XXX Existing check-it::int-generator misses generate method (lib is incomplete and abandoned):
;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   (defmethod check-it::generate ((g check-it::int-generator))
;;     (let ((low (or (check-it::lower-limit g) most-negative-fixnum))
;;           (high (or (check-it::upper-limit g) most-positive-fixnum)))
;;       (+ low (random (1+ (- high low)))))))

;; XXX Existing check-it::int-generator misses generate method (lib is incomplete and abandoned):
(defmethod check-it::generate ((g check-it::int-generator))
  (let ((low (or (check-it::lower-limit g) most-negative-fixnum))
        (high (or (check-it::upper-limit g) most-positive-fixnum)))
    (+ low (random (1+ (- high low))))))

(5am:test next-fname-never-stuck
          (5am:is
           (check-it
            fname-non-terminal-transitions-gen
            (lambda (vals)
              (let* ((st (first vals))
                     (ev (second vals))
                     (result (acl2::next-fname st ev)))
                (and (consp result)
                     (member (car result)
                             *valid-next-states*
                             :test #'eql)))))))

(defmacro it-exists (var gen-spec &body body)
  (let ((g (gensym "GEN"))
        (val (gensym "VAL")))
    `(let ((,g ,gen-spec))
       (ignore-errors
         (loop repeat check-it::*num-trials*
               for ,val = (generate ,g)
               when (let ((,var ,val)) (progn ,@body))
                 do (return ,val)
               finally (return nil))))))

(5am:test test-exists
  (5am:is (it-exists x (generator (tuple (integer 91 92) (integer 91 92))) ;;(bounded-int 91 92))
            ;; (format t "!!!!!!!!!!!!!!!!!!!! ~A~%" x)
            (> (car x) 90))))


;; Example how to define my own/custom generator (it does not cache values):
;; -------------------------------------------------------------------------
;; (defclass my-int-generator ()
;;   ((low  :initarg :low  :accessor low)
;;    (high :initarg :high :accessor high)))

;; (defmethod check-it::generate ((g my-int-generator))
;;   (+ (low g)
;;      (random (1+ (- (high g) (low g))))))

;; (defmethod check-it::cached-value ((g my-int-generator))
;;   (check-it::generate g))

;; How to test that it works:
;; --------------------------
;; (5am:test test-dump
;;   (check-it (make-instance 'my-int-generator :low 91 :high 92)
;;     (lambda (x)
;;       (format t "Generated: ~A~%" x)
;;       t)))


;; (5am:test next-fname-never-stuck1
;;   (5am:is (it-exists (x (integer 0 100))
;;             (> x 90))))
;; (5am:test next-fname-never-stuck
;;   (5am:exists
;;     (it-exists (x (integer 0 100))
;;       (> x 90))))

;; (defun make-state-event-generator ()
;;   (generator
;;    (lambda ()
;;      (list
;;       (nth (random 3) '(:decorations :dash :space))
;;       (nth (random 4) '(:decorations :dash :space :fnamechars))))))

;; (5am:test next-fname-never-stuck
;;   (5am:is
;;    (check-it
;;     '(tuple
;;      (generator (lambda () (nth (random 3) (:decorations :dash :space))))
;;      (generator (lambda () (nth (random 4) (:decorations :dash :space :fnamechars)))))
;;     (lambda (vals)
;;       (let* ((st (first vals))
;;              (ev (second vals))
;;              (result (acl2::next-fname st ev)))
;;         (and (consp result)
;;              (member (car result) *valid-next-states* :test #'eql)))))))






;; (5am:test next-fname-never-stuck
;;           (5am:is (check-it
;;                    (check-it::tuple
;;                      (generator (lambda () (nth (random 3) '(:decorations :dash :space))))
;;                      (generator (lambda () (nth (random 4) '(:decorations :dash :space :fnamechars)))))
;;                    (lambda (state event)
;;                      (let ((result (acl2::next-fname state event)))
;;                        (and (consp result)
;;                             (member (car result)
;;                                     *valid-next-states*
;;                                     :test #'eql)))))))

;; (5am:test next-fname-never-stuck
;;   ;; use check-it for property testing
;;   (5am:is (check-it:check-it
;;         "next-fname never produces invalid next states"
;;         (generator
;;           '(tuple
;;             (elements (:decorations :dash :space))  ; state
;;             (elements (:decorations :dash :space :fnamechars)))) ; event
;;         (let ((state (first values))
;;               (event (second values))
;;               (result (next-fname (first values) (second values))))
;;           ;; result must be a cons
;;           (and (consp result)
;;                (member (car result) *valid-next-states* :test #'eql))))))
