(in-package "ACL2")  ;; functions are provable in ACL2 and must be in this package to be recognized by ACL2

;; (defun make-symbol-like (base suffix)
;;   #+acl2
;;   (packn (list base suffix))
;;   #-acl2
;;   (intern (format nil "~A-~A" base suffix) (symbol-package base)))

#-acl2
(defvar *dbg-cond* nil)
#-acl2
(defun dbg (s &optional (x nil) &key (cond nil)) (when (or *dbg-cond* cond) (format t "~%*** TRACE[~A]: ~S~% ***~%" s x)) x)



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
  #+acl2
  (declare (xargs :guard (and (member-eq st *fname-terms*) (member-eq ev *next-fname-events*))))
  (cond
    ((and (eql st :decorations) (eql ev :dash))        (cons :dash nil))
    ((and (eql st :decorations) (eql ev :decorations)) (cons :decorations nil))
    ((and (eql st :decorations) (eql ev :space))       (cons :decorations nil))
    ((and (eql st :dash) (eql ev :space))              (cons :space nil))
    ((and (eql st :dash) (eql ev :dash))               (cons :dash nil))
    ((and (eql st :dash) (eql ev :fnamechars))         (cons :fnamechars t))
    ((and (eql st :space) (eql ev :fnamechars))        (cons :fnamechars t))
    (t                                                 (cons :unexpected nil))))

#+acl2
(defconst *spaces* (list (code-char 32)
                         (code-char 9)
                         (code-char 160)))
#-acl2
(defparameter *spaces* (list (code-char 32)
                             (code-char 9)
                             (code-char 160)))

(defun next-fname-event (ch)
  #+acl2
  (declare (xargs :guard (characterp ch)))
  (cond
    ((member ch (coerce "+`|│├└" 'list)) :decorations)
    ((member ch (coerce "─-" 'list)) :dash)
    ((member ch *spaces*) :space)
    ((not (member ch *spaces*)) :fnamechars)))

#+acl2
(defun split-string-at (lst n)
  (declare (xargs :guard (and (true-listp lst) (natp n))))
  (cons (take n lst)
        (nthcdr n lst)))

#-acl2
(declaim (ftype (function (string integer) cons) split-string-at))
#-acl2
(defun split-string-at (str num)
  "Returns (str-before-num . str-after-num) treating NUM index in the same way as Python does it"
  (let ((last-idx (length str)))
    (if (or (<= last-idx 0) (> num last-idx))
        nil
        (cons (subseq str 0 num) (subseq str num last-idx)))))

(defun empty-string-p (str)
  #+acl2 (declare (xargs :guard (true-listp str)))
  #+acl2 (endp str) ;; structurally recursive for ACL2
  #-acl2 (zerop (length str)))

(defun first-char (str)
  #+acl2 (declare (xargs :guard (true-listp str)))
  #+acl2 (car str)
  #-acl2 (char str 0))

(defun rest-chars (str)
  #+acl2 (declare (xargs :guard (true-listp str)))
  #+acl2 (cdr str)
  #-acl2 (subseq str 1))

;; XXX Extracts fname and indent from tree output line, but some indent should exist, else returns NIL
(defun extract-fname-and-indent-1 (str idx st)
  #+acl2
  (declare (xargs :guard (and (true-listp str) (integerp idx) (member-eq st *fname-terms*))))
  (if ;; XXX (endp ..) works for lists only, SBCL detects error, but ACL2 is happy.
      ;; XXX To make SBCL happy - we use (zerop (length ..)) but it breaks ACL2 logic.
      ;; XXX So, we use empty-string-p which has 2 implementations: for SBCL, ACL2.
      (empty-string-p str)
      (cons :unexpected 0)
      (let* ((ch (first-char str))
             (ev (next-fname-event ch))
             (trans (next-fname st ev))
             (new-st (car trans))
             (ended (cdr trans)))
        (cond (ended (cons new-st idx))
              ((= 1 (length str)) (cons new-st idx))
              (t (extract-fname-and-indent-1 (rest-chars str) (1+ idx) new-st))))))

(defun extract-fname-and-indent (str)
  #+acl2
  (declare (xargs :guard (true-listp str)))
  (let* ((final-trans (extract-fname-and-indent-1 str 0 :decorations))
         (final-st (car final-trans))
         (final-pos (cdr final-trans)))
    (case final-st
      (:fnamechars (cons (cdr (split-string-at str final-pos)) final-pos))
      (t nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ctx - context ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; #+acl2
;; (defprod ctx
;;     ((indents integer-listp)
;;      (fnames string-listp)
;;      (curdir stringp)))
#-acl2
(defstruct ctx
  (indents nil :type (or null list))
  (fnames nil :type (or null list))
  (curdir nil :type (or null string)))

#+acl2
(defun create-ctx (indents fnames)
  (list (cons :indents indents)
        (cons :fnames fnames)
        (cons :curdir nil)))
;; (defun create-ctx (indents fnames) (ctx indents fnames))

#-acl2
(defun create-ctx (indents fnames) (make-ctx :indents indents :fnames fnames))

#+acl2
;; (defun get-indents (ctx) (ctx->indents ctx))
(defun get-indents (ctx)
  (cdr (assoc-equal :indents ctx)))
#-acl2
(defun get-indents (ctx) (ctx-indents ctx))

(defun get-last-indent (ctx) (car (get-indents ctx)))

#+acl2
(defun get-fnames (ctx)
  (cdr (assoc-equal :fnames ctx)))
;; (defun get-fnames (ctx) (ctx->fnames ctx))
#-acl2
(defun get-fnames (ctx) (ctx-fnames ctx))

#+acl2
(defun get-curdir (ctx)
  (cdr (assoc-equal :curdir ctx)))
;; (defun get-curdir (ctx) (ctx->curdir ctx))
#-acl2
(defun get-curdir (ctx) (ctx-curdir ctx))

#+acl2
(defun change-ctx (ctx key val)
  (put-assoc-equal key val ctx))

#+acl2
(defun get-last-fname (ctx) (car (get-fnames ctx)))
#-acl2
(defun get-last-fname (ctx) (car (get-fnames ctx)))

#+acl2
(defun push-indent (ctx indent) (change-ctx ctx :indents (cons indent (get-indents ctx))))
#-acl2
(defun push-indent (ctx indent) (progn (setf (ctx-indents ctx) (cons indent (get-indents ctx))) ctx))

#+acl2
(defun push-fname (ctx fname) (change-ctx ctx :fnames (cons fname (get-fnames ctx))))
#-acl2
(defun push-fname (ctx fname) (progn (setf (ctx-fnames ctx) (cons fname (ctx-fnames ctx))) ctx))

(defun push-fname-and-indent (ctx fname indent)
  (let* ((ctx1 (push-fname ctx fname))
         (ctx2 (push-indent ctx1 indent)))
    ctx2))

#+acl2
(defun change-last-fname (ctx fname) (change-ctx ctx :fnames (cons fname (cdr (get-fnames ctx)))))
#-acl2
(defun change-last-fname (ctx fname) (progn (setf (ctx-fnames ctx) (cons fname (cdr (get-fnames ctx)))) ctx))

#+acl2
(defun set-curdir (ctx curdir) (change-ctx ctx :curdir curdir))
#-acl2
(defun set-curdir (ctx curdir) (progn (setf (ctx-curdir ctx) curdir) ctx))

(defun cmp-new-indent-with-old (ctx new-indent)
  (let ((indent-delta (- new-indent (or (get-last-indent ctx) 0))))
    (cond ((< 0 indent-delta) :to-right)
          ((> 0 indent-delta) :to-left)
          (t :same))))


(defun until-indent-p (crit-indent stack-indent) (= crit-indent stack-indent))
(def-pop-until pop-until-same-indent some-pop-until-same-indent-p :criteria until-indent-p) ;; TODO or directly eql


(defun pop-remembered-from-ctx-until-indent (ctx indent)
  (let* ((curr-indents (get-indents ctx))
         (curr-indents-num (length curr-indents))
         (new-indents (pop-until-same-indent curr-indents indent nil)) ;;t))
         (new-indents-num (length new-indents))
         (popped-num (- curr-indents-num new-indents-num))
         (new-fnames (nthcdr popped-num (get-fnames ctx))))
    (create-ctx new-indents new-fnames)))

(defun char-in-string-p (c str)
  #+acl2
  (declare (xargs :guard (and (stringp str)
                              (characterp c))))
  (member c (coerce str 'list)))

(defun string-left-trim2 (chars str start)
  #+acl2
  (declare (xargs :guard (and (stringp str)
                              (character-listp chars)
                              (integerp start))))
  (cond
    ((null str) nil)
    ((empty-string-p str) "")
    ((>= start (length str)) "")
    ((char-in-string-p (char str start) chars)
     (string-left-trim2 chars str (1+ start)))
    (t (subseq str start (length str)))))

(defun string-left-trim1 (chars str)
  #+acl2
  (declare (xargs :guard (and (stringp str)
                              (character-listp chars))))
  (string-left-trim2 chars str 0))

(defun string-right-trim1 (chars str)
  #+acl2
  (declare (xargs :guard (and (stringp str)
                              (character-listp chars))))
  (let* ((rev (coerce (reverse (coerce str 'list)) 'string))
         (trimmed (string-left-trim2 chars rev 0)))
    (coerce (reverse (coerce trimmed 'list)) 'string)))


(defun join-path-components (lst)  ;; TODO (join-path-components '("" "" "")) fails
  "Joins path components from left to right avoiding double /"
  #+acl2
  (declare (xargs :guard (string-listp lst)))
  (case (length lst)
    (0 nil)
    (1 (car lst))
    (t (if (car lst)
           (concatenate 'string
                        (string-right-trim1 '(#\/) (car lst))
                        "/"
                        (string-left-trim1 '(#\/)
                                           (join-path-components (cdr lst))))
           nil))))

(defun remove-empty-string-but-first1 (lst first)
  #+acl2
  (declare (xargs :guard (and (string-listp lst)
                              (booleanp first))))
  (cond
    ((null lst) nil)
    ((and (empty-string-p (car lst)) (not first)) (remove-empty-string-but-first1 (cdr lst) nil))))

(defun remove-empty-string-but-first (lst)
  "Removes all empty strings but the first one (if LST are path components, the first one is for the root)"
  #+acl2
  (declare (xargs :guard (string-listp lst)))
  (remove-empty-string-but-first1 lst t))
  ;; (loop for item in lst  ;; FIXME not acl2
  ;;         for i from 1
  ;;         if (or (string/= item "")
  ;;                (= i 1))
  ;;           collect item))

;; TODO add tests for it:
(defun add-fname-to-fnames-path (path fnames)
  "Adds a path to fnames stack (so FNAMES are passed reversed - the last one is in the head of the list) and returns
them as a path-string"
  (let* ((path-len (length path))
         (last-path-ch (if (> path-len 0)
                           (char path (1- path-len))
                           nil))
         (norm-path (if (and last-path-ch
                             (char-equal last-path-ch #\/))
                        (concatenate 'string
                                     (string-right-trim1 '(#\/) path)
                                     "/")
                        path)))
    (join-path-components (reverse (cons norm-path fnames)))))

(defun unquote-string (str)
  "Returns the string without quotes if it is wrapped in double quotes."
  (if (and (>= (length str) 2)
           (char-equal (char str 0) #\")
           (char-equal (char str (- (length str) 1)) #\"))
      (concatenate 'string
                   (subseq str 1 (- (length str) 1)))
      str))


;; From man tree(1):
;;   Append a `/' for directories, a `=' for socket files, a `*' for executable files, a `>' for doors (Solaris) and
;;   a `|' for FIFO's, as per ls -F
#-acl2
(declaim (ftype (function (character) (member :directory :socket :executable :door :fifo :file)) classify-by-last-char))
(defun classify-by-last-char (fname-last-ch)
  (case fname-last-ch
    (#\/ :directory)
    (#\= :socket)
    (#\* :executable)
    (#\> :door)
    (#\| :fifo)
    (t   :file)))

#-acl2
(declaim (ftype (function (string integer (or null integer)) (or null integer)) find-last-path-sep))
(defun find-last-path-sep (str i found-i)
  "Finds the index of the last / in STR. I must be 0, FOUND-I - NIL in the first call"
  (let ((len (length str)))
    (cond ((>= i len) found-i)
          ((eql #\/ (char str i)) (find-last-path-sep str (1+ i) i))
          (t (find-last-path-sep str (1+ i) found-i)))))

#-acl2
(declaim (ftype (function (string) (or null string)) path-basename))
(defun path-basename (str)
  "Returns the last path part in this way:
/ -> nil;  aaa/bbb -> bbb;  /aaa -> aaa;  aaa/ -> aaa/;  aaa -> aaa"
  (let* ((str1 (string-right-trim1 '(#\/) str))
         (len (length str))
         (last-sep (find-last-path-sep str1 0 nil)))
    (cond ((and (null last-sep)
                (or (null str)
                    (empty-string-p str)))
           nil)
          ((null last-sep) str)
          (t (subseq str (1+ last-sep) len)))))

#-acl2
(declaim (ftype (function (string)
                          (or null (member :directory :socket :executable :door :fifo :file)))
                classify-by-last-char-in-str))
(defun classify-by-last-char-in-str (str)
  "Similar to CLASSIFY-BY-LAST-CHAR but getting STRING as arg. Returns NIL if STR is empty"
  (let* ((str1 (path-basename str))
         (len (length str1)))
    (case len
      (0 nil)
      (1 :file)
      (t (classify-by-last-char (char str1 (1- len)))))))

(defun adapt-qf-fname (str)
  "Adapts fname (file or directory name, not path, even relative): tree(1) -F returns a name
with optional ending symbol /,=,*,>,| classifying the file. This function returns the fname
without this symbol and the class of the file - as a cons. When STR is empty, returns nil
instead of fname. Classes are - :EMPTY, :FILE, :DIRECTORY, :SOCKET, :EXECUTABLE, :DOOR, :FIFO"
  (let* ((unquoted-str (unquote-string str))
         (unquoted-len (length unquoted-str))
         (len (length str))
         (last-i (1- len)))
    (case unquoted-len
      (0 (cons nil :empty))
      (1 (cons unquoted-str :file))
      (t (let* ((last-ch (char str last-i))
                (str-wo-symbol (unquote-string (subseq str 0 last-i)))
                (cls (classify-by-last-char last-ch))
                (fname (case cls
                         (:file unquoted-str)
                         (t str-wo-symbol))))
           (cons fname cls))))))

(defun extract-fname-and-indent-including-zero-indent (line-num str)
  "Extracts fname and indent as a cons. STR is a string from tree(1) which can be quoted and
classified (see -Q and -F CLI options respectively): both cases are supported. But -QF is
preferred - it allows to determine directories and spaces (and other strange symbols) in the name.

Keeps only :file, :directory entries, others - dropped!"
  (let* ((fname-and-indent (if (= 1 line-num)
                               (cons str 0)
                               (extract-fname-and-indent str)))
         (fname (car fname-and-indent))
         (indent (cdr fname-and-indent))
         (adapted-fname (adapt-qf-fname fname))
         (fname1 (car adapted-fname))
         (what (cdr adapted-fname)))
    (case what
      (:file (cons fname1 indent))
      (:directory (cons (concatenate 'string fname1 "/") indent))
      (t nil))))

(defun parse-tree-output-1 (ctx lines line-num results)
  "Keeps only :file, :directory entries, see EXTRACT-FNAME-AND-INDENT-INCLUDING-ZERO-INDENT"
  (let ((rest-lines (cdr lines)))
    (if lines
        (let* ((line (car lines))
               (fname-and-indent (extract-fname-and-indent-including-zero-indent line-num line)))
          ;; (dbg (format nil "!!!!!~A: line = ~A  |  fname-and-indent = ~X  |  results = ~X  |  ctx = ~X"
          ;;              line-num line fname-and-indent results ctx))
          (if fname-and-indent
              (let ((fname (car fname-and-indent))
                    (indent (cdr fname-and-indent)))   ;destructuring-bind (fname . indent) fname-and-indent
                (if (= 1 line-num)
             ;;;;;;;;;;;;;;;;;;;;; line-num = 1:
                    (let* ((path (add-fname-to-fnames-path fname (get-fnames ctx)))
                           (ctx1 ctx) ;;(push-indent ctx 0))  ;; TODO added
                           (ctx2 (set-curdir ctx1 fname)))
                      (parse-tree-output-1 ctx2 rest-lines (1+ line-num) (cons path results)))
                    ;; XXX 1st line does not push indent, so we cannot find it later pushed in stack!!!
             ;;;;;;;;;;;;;;;;;;;;; else line-num != 1:
                    (case (cmp-new-indent-with-old ctx indent)
                      (:to-left
                       (let* ((ctx1 (pop-remembered-from-ctx-until-indent ctx indent))
                              (path (add-fname-to-fnames-path fname (get-fnames ctx1)))
                              (ctx2 (set-curdir ctx1 fname)))
                         (parse-tree-output-1 ctx2 rest-lines (1+ line-num) (cons path results))))
                      (:to-right
                       (let* (;; XXX if tree(1) was ran with -QF, we see dirs as "..."/ and set them in RESULTS
                              ;; XXX as directories, but w/o -F, dirs are w/o trailing "/" and when we shift to
                              ;; XXX right, then the previous result in RESULTS was a directory, w/o "/". Here
                              ;; XXX we fix it changing the last result in RESULTS, so we are able to work when
                              ;; XXX tree(1) running w/o -F: in this case empty dirs will not lead shift :TO-RIGHT
                              ;; XXX and we will not fix them in RESULTS and they will be ensured as files (!).
                              ;; XXX But it's for empty dirs only. To fix it, use -F flag of tree(1):
                              (last-result (car results))
                              (last-result-as-dir (join-path-components (list last-result "")))
                              (fixed-results (cons last-result-as-dir (cdr results)))
                              ;; push current directory to fnames stack etc...
                              (ctx1 (push-fname ctx (get-curdir ctx)))
                              (ctx2 (push-indent ctx1 indent))
                              (path (add-fname-to-fnames-path fname (get-fnames ctx2)))
                              (ctx3 (set-curdir ctx2 fname)))
                         (parse-tree-output-1 ctx3 rest-lines (1+ line-num) (cons path fixed-results))))
                      (:same
                       (let* ((path (add-fname-to-fnames-path fname (get-fnames ctx)))
                              (ctx1 (set-curdir ctx fname)))
                         (parse-tree-output-1 ctx1 rest-lines (1+ line-num) (cons path results)))))))
              (parse-tree-output-1 ctx rest-lines (1+ line-num) results)))
        results)))

#-acl2
(declaim (ftype (function (list) list) parse-tree-output))
(defun parse-tree-output (lines)
  (reverse (parse-tree-output-1 (create-ctx nil nil) lines 1 nil)))

#-acl2
(declaim (ftype (function (list boolean) boolean) symbols-form-special-path-1-p))
(defun symbols-form-special-path-1-p (chars-list first-call)
  (cond ((member (car chars-list) '(#\/ #\.))
         (symbols-form-special-path-1-p (cdr chars-list) nil))
        ((null chars-list) (not first-call))
        (t nil)))

#-acl2
(declaim (ftype (function (string) boolean) symbols-form-special-path-p))
(defun symbols-form-special-path-p (str)
  "Checks that all symbols of STR forms a special path (XXX does not need creation)
like / or ../.. - this check is not perfect bcs it treats ..... as a special
path (it's weird, but not a special in a sense that it doesn ot need to be created)"
  (symbols-form-special-path-1-p (coerce str 'list) t))


#-acl2
(declaim (ftype (function (list list) list) classify-tree-output1))
(defun classify-tree-output1 (lines results)
  "This function gets already clean lines in sense that they passed ADAPT-QF-FNAME and
they are unquoted and classified by that function, here lines correspond to :FILE, :DIRECTORY only,
other classes are removed. Directory lines end with /"
  (if (null lines)
      results
      (let* ((line0 (car lines))
             (what (classify-by-last-char-in-str line0))
             (cmd (cond
                    ((symbols-form-special-path-p line0)
                     (list :special line0))
                    ((eq what :file)
                     (list :file line0))
                    ((eq what :directory)
                     (list :directory line0))
                    (t nil))))
        (if (null cmd)
            (classify-tree-output1 (cdr lines) results)
            (classify-tree-output1 (cdr lines) (cons cmd results))))))

#-acl2
(declaim (ftype (function (list) list) classify-tree-output))
(defun classify-tree-output (lines)
  "Classifies a list of strings (lines read from tree command output) - what kind of entry is
every line, see CLASSIFY-TREE-OUTPUT1"
  (reverse (classify-tree-output1 lines nil)))

#-acl2
(declaim (ftype (function (string string) string) prepend-fname))
(defun prepend-fname (new-root fname)
  "Changes the directory of FNAME"
  (concatenate 'string (string-right-trim1 '(#\/) new-root) "/" (string-left-trim1 '(#\/ #\.) fname)))

#-acl2
(declaim (ftype (function (string list boolean list) list) prepend-classified-tree-output1))
(defun prepend-classified-tree-output1 (new-root classified-lines first-call result)
  (if (null classified-lines)
      (if first-call
          result
          (append result
                  ;; prepended prefix can be missing dir - it must be created too, we add it
                  ;; at the very tail - anyway the result will be reversed in PREPEND-CLASSIFIED-TREE-OUTPUT:
                  (list (list :directory
                              (prepend-fname (string-right-trim1 '(#\/) new-root)
                                             "")))))
      (let* ((classified-line0 (car classified-lines))
             ;; classified-line0 is (<CLASS> fname) where CLASS is :file|:directory|:special
             (classified-line0-class (car classified-line0))
             (classified-lines-rest (cdr classified-lines))
             (classified-line0-fname (cadr classified-line0))
             (classified-line0-prepended-fname (case classified-line0-class
                                                 (:special classified-line0-fname)
                                                 (t (prepend-fname new-root classified-line0-fname)))))
        (prepend-classified-tree-output1 new-root
                                         classified-lines-rest
                                         nil
                                         (cons (list classified-line0-class
                                                     classified-line0-prepended-fname)
                                               result)))))

#-acl2
(declaim (ftype (function (string list) list) prepend-classified-tree-output))
(defun prepend-classified-tree-output (new-root classified-lines)
  (reverse (prepend-classified-tree-output1 new-root classified-lines t nil)))

#-acl2
(declaim (ftype (function (list list list list) list) escape-path-spec-symbols-1))
(defun escape-path-spec-symbols-1 (str-chars unsafe-chars escaping-chars result)
  (cond
    ((null str-chars) result)
    ((member (car str-chars) unsafe-chars)
     (escape-path-spec-symbols-1 (cdr str-chars)
                                 unsafe-chars
                                 escaping-chars
                                 (append result escaping-chars (list (car str-chars)))))
    (t (escape-path-spec-symbols-1 (cdr str-chars)
                                   unsafe-chars
                                   escaping-chars
                                   (append result (list (car str-chars)))))))

#-acl2
(declaim (ftype (function (string) string) escape-path-spec-symbols))
(defun escape-path-spec-symbols (str)
  (coerce (escape-path-spec-symbols-1 (coerce str 'list) '(#\* #\\ #\?) '(#\\) nil) 'string))

#-acl2
(declaim (ftype (function (string) string) escape-shell-spec-symbols))
(defun escape-shell-spec-symbols (str)
  (coerce (escape-path-spec-symbols-1 (coerce str 'list) '(#\') '(#\' #\" #\' #\") nil) 'string))
