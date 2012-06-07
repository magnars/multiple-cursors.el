(And "^delete-selection-mode is active$"
     (lambda ()
        (delete-selection-mode 1)))

(defun is-extra-cursor-p (o)
  (eq (overlay-get o 'type) 'additional-cursor))

(defun num-cursors ()
  (1+ (count-if 'is-extra-cursor-p
            (overlays-in (point-min) (point-max)))))

(Then "^I should have 2 cursors$"
       (lambda ()
         (assert (eq 2 (num-cursors)) nil
                 "Expected to have 2 cursors, but was %d." (num-cursors))))
