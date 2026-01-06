(in-package "ACL2")

(include-book "utils")

(def-pop-until pop-until-equal-zero some-item-zerop :criteria zerop)

(defthm pop-until-zero-never-expands
  (<= (len (pop-until-equal-zero stack include))
      (len stack)))

(defthm pop-until-zero-shrinks-if-elem-exists
  (implies (some-item-zerop stack)  ;; if item is in the stack
           (< (len (pop-until-equal-zero stack t))
              (len stack))))

(defthm next-fname-start-multi-decorations
    (implies (and (member-eq st '(:decorations :dash))
                  (eql st ev))
             (let* ((result (next-fname-start st ev))
                    (new-st (car result))
                    (ended-p (cdr result)))
               (and (eql new-st st)
                    (null ended-p)))))
