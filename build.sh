#!/usr/bin/env bash
set -euo pipefail

LISP_CMD=${1:-sbcl}
NAME="easytree"
LOGFILE=build.log
: > "$LOGFILE"

# Increase heap if your image is large (adjust MB)
DYNAMIC_SPACE=${DYNAMIC_SPACE:-4096}
# runtime vs lisp options
SBCL_RUNTIME_OPTS=(--dynamic-space-size "$DYNAMIC_SPACE")
SBCL_LISP_OPTS=(--noinform --no-sysinit --noprint)

if [ "${RELEASE+x}" = x ]; then
  SBCL_LISP_OPTS+=(--disable-debugger)
  SAVE_EXTRA=":purify t :compression t"
else
  SBCL_LISP_OPTS+=() #--debug 3)
  SAVE_EXTRA=""
fi

read -r -d '' LISP_CODE <<'LISP' || true
(progn
  (require "asdf")
  (defun write-log (s)
    (with-open-file (out "/tmp/easytree-build.log"
                         :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      (format out "~A~%" s) (finish-output)))
  (write-log "=== build started ===")
  (when (fboundp 'asdf:clear-source-registry)
    (write-log "clearing source registry")
    (asdf:clear-source-registry)
    (asdf:initialize-source-registry))
  (write-log (format nil "find-system: ~S" (asdf:find-system :easytree)))
  (write-log "about to asdf:load-system :easytree")
  (asdf:load-system :easytree)
  (write-log "asdf:load-system returned")
  (let ((toplevel-sym (or (find-symbol "MAIN" "EASYTREE")
                          (intern "MAIN" "EASYTREE"))))
    (write-log (format nil "toplevel symbol: ~S" toplevel-sym))
    (write-log "about to save-lisp-and-die")
    (sb-ext:save-lisp-and-die "easytree"
      :executable t
      :save-runtime-options t
      :toplevel toplevel-sym
      SAVE-EXTRA)))
LISP

LISP_CODE="${LISP_CODE//SAVE-EXTRA/$SAVE_EXTRA}"

echo "---- Lisp code to be evaluated ----" | tee -a "$LOGFILE"
printf '%s\n' "$LISP_CODE" | tee -a "$LOGFILE"
echo "---- end of code ----" | tee -a "$LOGFILE"

echo "$LISP_CODE" > /tmp/easytree-build.lisp

# Run under qlot exec if you rely on qlot local projects
echo "About to run $LISP_CMD ${SBCL_RUNTIME_OPTS[@]} ${SBCL_LISP_OPTS[@]} --load /tmp/easytree-build.lisp" | tee -a "$LOGFILE"
$LISP_CMD "${SBCL_RUNTIME_OPTS[@]}" "${SBCL_LISP_OPTS[@]}" --load /tmp/easytree-build.lisp |tee -a "$LOGFILE" 2>&1
