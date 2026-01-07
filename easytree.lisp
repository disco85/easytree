(declaim (optimize (debug 3) (safety 3) (speed 0)))

(in-package #:easytree)

;; (defparameter *unicode-whitespace-chars*
;;   '(#\Space #\Tab #\Newline #\Return
;;     #\No-break_space           ; U+00A0
;;     #\Ogham_space_mark         ; U+1680
;;     #\En_quad                  ; U+2000
;;     #\Em_quad                  ; U+2001
;;     #\En_space                 ; U+2002
;;     #\Em_space                 ; U+2003
;;     #\Three-per-em_space       ; U+2004
;;     #\Four-per-em_space        ; U+2005
;;     #\Six-per-em_space         ; U+2006
;;     #\Figure_space             ; U+2007
;;     #\Punctuation_space        ; U+2008
;;     #\Thin_space               ; U+2009
;;     #\Hair_space               ; U+200A
;;     #\Zero_width_space         ; U+200B
;;     #\Narrow_no-break_space    ; U+202F
;;     #\Medium_mathematical_space; U+205F
;;     #\Ideographic_space))
                                        ; U+3000

(defparameter *unicode-whitespace-chars*
  (list #\Space #\Tab #\Newline #\Return
        (code-char #x00A0)
        (code-char #x1680)
        (code-char #x2000)
        (code-char #x2001)
        (code-char #x2002)
        (code-char #x2003)
        (code-char #x2004)
        (code-char #x2005)
        (code-char #x2006)
        (code-char #x2007)
        (code-char #x2008)
        (code-char #x2009)
        (code-char #x200A)
        (code-char #x200B)
        (code-char #x202F)
        (code-char #x205F)
        (code-char #x3000)))

(defparameter *unicode-whitespace-string* (coerce *unicode-whitespace-chars* 'string))

;; (deftype <fname-terms> (append '(member) acl2::*fname-terms*))
(defun fname-term-p (x) (member x acl2:*fname-terms* :test #'eql))
(deftype <fname-term> () `(satisfies fname-term-p))

(declaim (ftype (function (character) <fname-term>) acl2:next-fname-event))
;; (defun next-fname-event (ch)
;;   (cond
;;     ((find ch "+`├") :decorations)
;;     ((find ch "─-") :dash)
;;     ((find ch " 	") :space)
;;     ((not (find ch " 	")) :fnamechars)))


(defparameter *OS* nil)

(defun trim-any-whitespaces (string)
  "Like Python's str.strip()"
  (string-trim *unicode-whitespace-chars* string))

(defun det-*OS* ()
  (cond ((member :windows *features*) :windows)
        ((member :unix *features*)
         (cond ((member :darwin *features*) :mac)
               (t :unix)))
        (t (error "OS not recognized!"))))

;; Path separator (as a string)
(defparameter *path-sep* nil)

(defun det-*path-sep* ()
  (ecase *OS* (:windows "\\") (:unix "/") (:mac "/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Command line ;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cli-script-cmd-handler (cmd)
  (declare (ignorable cmd))
  (let* ((force-fmode (clingon:getopt cmd :fmode))
         (strip (clingon:getopt cmd :strip))
         (new-root (clingon:getopt cmd :prepend)))
    (loop for line = (read-line *standard-input* nil nil)
          while line
          do
             (setf line (string-trim *unicode-whitespace-chars* line))
             (unless (emptyp line)
               (format t "Parse line: ~A~%" line))
          :finally
             (format t "End"))))

(defun cli-make-cmd-handler (cmd)
  (declare (ignorable cmd))
  (let* ((force-fmode (clingon:getopt cmd :fmode))
         (strip (clingon:getopt cmd :strip))
         (dry (clingon:getopt cmd :dry))
         (verbose (clingon:getopt cmd :verbose))
         (new-root (clingon:getopt cmd :prepend)))
    (loop for line = (read-line *standard-input* nil nil)
          while line
          do
             (setf line (string-trim *unicode-whitespace-chars* line))
             (unless (emptyp line)
               (format t "Parse line: ~A~%" line))
          :finally
             (format t "End"))))

(defun cli-script-cmd-opts ()
  (list
   (clingon:make-option
    :flag
    :description "Set permissions (file mode) too"
    :short-name #\m
    :long-name "fmode"
    :key :fmode)
   (clingon:make-option
    :flag
    :description "Verbose"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)
   (clingon:make-option
    :integer
    :description "Strip paths (N dirs up)"
    :short-name #\s
    :long-name "strip"
    :initial-value 0
    :key :strip)
   (clingon:make-option
    :string
    :description "Prepend paths with common directory-prefix"
    :short-name #\p
    :long-name "prepend"
    :key :prepend)))

(defun cli-make-cmd-opts ()
  (list
   (clingon:make-option
    :flag
    :description "Set permissions (file mode) too"
    :short-name #\m
    :long-name "fmode"
    :key :fmode)
   (clingon:make-option
    :flag
    :description "Verbose"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)
   (clingon:make-option
    :flag
    :description "Dry run"
    :short-name #\d
    :long-name "dry"
    :key :dry)
   (clingon:make-option
    :integer
    :description "Strip paths (N dirs up)"
    :short-name #\s
    :long-name "strip"
    :initial-value 0
    :key :strip)
   (clingon:make-option
    :string
    :description "Prepend paths with common directory-prefix"
    :short-name #\p
    :long-name "prepend"
    :key :prepend)))

(defun cli-tests-cmd ()
  (clingon:make-command
   :name "tests"
   :usage "<no args>"
   :description "Run tests"
   :handler #'cli-tests-handler))

(defun cli-script-cmd ()
  (clingon:make-command
   :name "script"
   :usage "[-s N] [-p DIR] [-v] [-m]"
   :examples '(("Generate shell script:" . "script")
               ("Generate shell script prepending it with a directory:" . "script -p x/y/z")
               ("Generate shell script striping a directory by 2 levels:" . "script -s 2")
               ("Generate shell script with mode setting" . "script -m"))
   :description "Generate a Linux shell script reproducing the directory tree"
   :handler #'cli-script-cmd-handler
   :options (cli-script-cmd-opts)))

(defun cli-make-cmd ()
  (clingon:make-command
   :name "make"
   :usage "[-s N] [-p DIR] [-v] [-m] [-d]"
   :examples '(("Make a tree:" . "script")
               ("Make a tree prepending it with a directory:" . "script -p x/y/z")
               ("Make a tree striping a directory by 2 levels:" . "script -s 2")
               ("Make a tree with mode setting" . "script -m")
               ("Simulate making a tree (dry mode)" . "script -d"))
   :description "Reproduce (make) directories and files"
   :handler #'cli-make-cmd-handler
   :options (cli-make-cmd-opts)))


(defun cli-main-cmd ()
  (clingon:make-command
   :name "tree"
   :description "File system tree reproducing"
   :version "0.1.0"
   :authors '("John Doe <john.doe@example.org>")
   :license "BSD 2-Clause"
   :sub-commands (list (cli-script-cmd) (cli-make-cmd))
   :handler (lambda (cmd)
              (format t "No known subcommand provided!~%~%")
              (clingon:print-usage cmd *standard-output*)
              (uiop:quit 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
  (let* ((*OS* (det-*OS*))
         (*path-sep* (det-*path-sep*))
         (argv (uiop:command-line-arguments)))
    (clingon:run (cli-main-cmd) argv)))

