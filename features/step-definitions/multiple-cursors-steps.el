(And "^delete-selection-mode is active$"
     (lambda ()
        (delete-selection-mode 1)))

(defun is-extra-cursor-p (o)
  (message "overlay-type: %S" (overlay-get o 'type))
  (eq (overlay-get o 'type) 'additional-cursor))

(defun num-cursors ()
  (1+ (count-if 'is-extra-cursor-p
            (overlays-in (point-min) (point-max)))))

(Then "^I should have \\([0-9]+\\) cursors$"
       (lambda (num)
         (assert (eq (string-to-number num) (num-cursors)) nil
                 "Expected to have %s cursors, but was %d." num (num-cursors))))
