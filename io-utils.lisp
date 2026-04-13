(in-package :easytree)

(declaim (ftype (function (string boolean boolean) (values null &optional)) ensure-file))
(defun ensure-file (file-path dry-run verbose-run)
  (let ((escaped-file-path (acl2::escape-path-spec-symbols file-path))) ;; TODO export it and use acl2:
    (if dry-run
        (format t "[DRY] Creating file ~A ...~%" escaped-file-path)
        (unless (probe-file escaped-file-path)
          (when verbose-run (format t "Creating file ~A ...~%" escaped-file-path))
          (with-open-file (s escaped-file-path
                             :direction :output
                             :if-does-not-exist :create)
            nil)))))

(declaim (ftype (function (string boolean boolean) (values null &optional)) ensure-dir))
(defun ensure-dir (dir-path dry-run verbose-run)
  (let ((escaped-dir-path (acl2::escape-path-spec-symbols dir-path)))  ;; TODO export it and use acl2:
    (if dry-run
        (format t "[DRY] Creating directory ~A ...~%" dir-path)
        (unless (uiop:directory-exists-p escaped-dir-path)
          (when verbose-run (format t "Creating directory ~A ...~%" escaped-dir-path))
          (ensure-directories-exist escaped-dir-path)
          nil))))

(declaim (ftype (function (string boolean boolean) (values null &optional)) script-ensuring-file))
(defun script-ensuring-file (file-path dry-run verbose-run)
  (let ((escaped-file-path (acl2::escape-shell-spec-symbols file-path))) ;; TODO export it and use acl2:
    (if dry-run
        (format t "[ ! -e '~A' ] && echo '[DRY] Creating file ~A ...'~%" escaped-file-path escaped-file-path)
        (format t "[ ! -e '~A' ] && { ~@[echo 'Creating file ~A ...'; ~]touch '~A'; }~%"
                escaped-file-path (and verbose-run escaped-file-path) escaped-file-path))))

(declaim (ftype (function (string boolean boolean) (values null &optional)) script-ensuring-dir))
(defun script-ensuring-dir (dir-path dry-run verbose-run)
  (let ((escaped-dir-path (acl2::escape-path-spec-symbols dir-path)))  ;; TODO export it and use acl2:
  (if dry-run
      (format t "[ ! -e '~A' ] && echo '[DRY] Creating directory ~A ...'~%" escaped-dir-path escaped-dir-path)
      (format t "[ ! -e '~A' ] && { ~@[echo 'Creating directory ~A ...'; ~]mkdir -p '~A'; }~%"
              escaped-dir-path (and verbose-run escaped-dir-path) escaped-dir-path))))


(declaim (ftype (function (list keyword boolean boolean) null) ensure-classified-tree-output-line))
(defun ensure-classified-tree-output-line (classified-line how-to-ensure dry-run verbose-run)
  "Ensures (creates actually or as a script) file objects"
  (let* ((classified-line-class (car classified-line))
         (fname (cadr classified-line)))
    (case classified-line-class
      (:file (case how-to-ensure
               (:script (script-ensuring-file fname dry-run verbose-run))
               (:immediately (ensure-file fname dry-run verbose-run))))
      (:directory (case how-to-ensure
               (:script (script-ensuring-dir fname dry-run verbose-run))
               (:immediately (ensure-dir fname dry-run verbose-run)))))))


(declaim (ftype (function (list keyword boolean boolean) null) ensure-classified-tree-output))
(defun ensure-classified-tree-output (classified-lines how-to-ensure dry-run verbose-run)
  "Ensures (creates actually or as a script) file objects"
  (dolist (classified-line classified-lines)
    (ensure-classified-tree-output-line classified-line how-to-ensure dry-run verbose-run)))


(declaim (ftype (function () list) read-stdin-lines))
(defun read-stdin-lines ()
  (loop for line = (read-line *standard-input* nil nil)
        while line
        collect line))
