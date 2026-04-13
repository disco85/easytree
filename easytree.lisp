(declaim (optimize (debug 3) (safety 3) (speed 0)))

(in-package :easytree)

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

;; (declaim (ftype (function (character) <fname-term>) acl2:next-fname-event))
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
  (let* ((dry-run (clingon:getopt cmd :dry))
         (verbose-run (clingon:getopt cmd :verbose))
         (prepend-with (clingon:getopt cmd :prepend))
         (input-lines (read-stdin-lines))
         (parsed-lines (acl2::parse-tree-output input-lines))
         (classified-lines (acl2::classify-tree-output parsed-lines))
         (prepended-classified-lines
           (if prepend-with
               (acl2::prepend-classified-tree-output prepend-with classified-lines)
               classified-lines)))
    ;; (format t "~{~A~%~}~%" input-lines)
    (ensure-classified-tree-output prepended-classified-lines :script dry-run verbose-run)))

(defun cli-make-cmd-handler (cmd) ;; TODO put common code of this and prev to separate func
  (declare (ignorable cmd))
  (let* ((dry-run (clingon:getopt cmd :dry))
         (verbose-run (clingon:getopt cmd :verbose))
         (prepend-with (clingon:getopt cmd :prepend))
         (input-lines (read-stdin-lines))
         (parsed-lines (acl2::parse-tree-output input-lines))
         (classified-lines (acl2::classify-tree-output parsed-lines))
         (prepended-classified-lines
           (if prepend-with
               (acl2::prepend-classified-tree-output prepend-with classified-lines)
               classified-lines)))
    ;; (format t "~{~A~%~}~%" prepended-classified-lines)
    (ensure-classified-tree-output prepended-classified-lines :immediately dry-run verbose-run)))

(defun cli-script-cmd-opts ()
  (list
   (clingon:make-option
    :string
    :description "Prepend paths with common directory-prefix"
    :short-name #\p
    :long-name "prepend"
    :key :prepend)
   (clingon:make-option
    :flag
    :description "Dry run"
    :short-name #\d
    :long-name "dry"
    :key :dry)
   (clingon:make-option
    :flag
    :description "Verbose run"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)))

(defun cli-make-cmd-opts ()
  (list
   (clingon:make-option
    :flag
    :description "Dry run"
    :short-name #\d
    :long-name "dry"
    :key :dry)
   (clingon:make-option
    :string
    :description "Prepend paths with common directory-prefix"
    :short-name #\p
    :long-name "prepend"
    :key :prepend)
   (clingon:make-option
    :flag
    :description "Verbose run"
    :short-name #\v
    :long-name "verbose"
    :key :verbose)))

(defun cli-script-cmd ()
  (clingon:make-command
   :name "script"
   :usage "[-p DIR] [-d] [-v]"
   :examples '(("Generate shell script:" . "script")
               ("Generate shell script prepending it with a directory:" . "easytree -p x/y/z")
               ("Simulate making a tree (dry mode)" . "easytree -d")
               ("Run in verbose mode" . "easytree -v"))
   :description "Generate a Linux shell script reproducing the directory tree"
   :handler #'cli-script-cmd-handler
   :options (cli-script-cmd-opts)))

(defun cli-make-cmd ()
  (clingon:make-command
   :name "make"
   :usage "[-p DIR] [-d] [-v]"
   :examples '(("Make a tree:" . "script")
               ("Make a tree prepending it with a directory:" . "easytree -p x/y/z")
               ("Simulate making a tree (dry mode)" . "easytree -d")
               ("Run in verbose mode" . "easytree -v"))
   :description "Reproduce (make) directories and files"
   :handler #'cli-make-cmd-handler
   :options (cli-make-cmd-opts)))


(defun cli-main-cmd ()
  (clingon:make-command
   :name "tree"
   :description "File system tree reproducing"
   :version "0.1.0"
   :authors '("John Doe <john.doe@example.org>")
   :license "GPL-3.0-or-later"
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
    (clingon:run (cli-main-cmd) argv)
    ;; (print "Hello world")
    ))
