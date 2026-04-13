#!/usr/bin/env bash
set -euo pipefail

ACL2_CMD=${1:-~/apps/acl2-8.6/saved_acl2}
LOGFILE=verify.log
: > "$LOGFILE"

for f in acl2-*.lisp; do
    echo "==== Verify file $f ====" | tee -a "$LOGFILE"
    "$ACL2_CMD" < "$f" | tee -a "$LOGFILE"
    if grep -iq "Error" "$LOGFILE"; then
        echo
        echo ":(  Error (syntax?) in file $f!" | tee -a "$LOGFILE"
        exit 2
    elif grep -iq "\*\*\*\* FAILED \*\*\*\*" "$LOGFILE"; then
        echo
        echo ":(  ACL2 failed on file $f!" | tee -a "$LOGFILE"
        exit 1
    else
        echo
        echo ":)  ACL2 succeeded on file $f" | tee -a "$LOGFILE"
    fi
done
