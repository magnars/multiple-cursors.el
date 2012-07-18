(And "^delete-selection-mode is active$"
     (lambda ()
       (delete-selection-mode 1)))

(defun is-extra-cursor-p (o)
  (eq (overlay-get o 'type) 'additional-cursor))

(defun num-cursors ()
  (1+ (count-if 'is-extra-cursor-p
                (overlays-in (point-min) (point-max)))))

(Then "^I should have \\([0-9]+\\) cursors$"
      (lambda (num)
        (let ((actual (num-cursors)))
          (assert (eq (string-to-number num) actual) nil
                  "Expected to have %s cursors, but was %d." num actual))))

(Then "^I should have one cursor$"
      (lambda ()
        (assert (not multiple-cursors-mode) nil
                "Expected to have one cursor, but multiple-cursors-mode is still active.")
        (assert (eq 1 (num-cursors)) nil
                "Expected to have one cursor, but there are still fake cursor overlays.")))

(And "^I switch to multiple-cursors mode$"
     (lambda ()
       (mc/switch-from-mark-multiple-to-cursors)))
