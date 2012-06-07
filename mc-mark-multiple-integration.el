(require 'mark-multiple)

(defun mc/switch-from-mark-multiple-to-cursors ()
  "Removes mark-multiple and switches to multiple cursors instead"
  (interactive)
  (let ((offset (- (point) (overlay-start mm/master))))
    (deactivate-mark)
    (save-excursion
      (dolist (mirror mm/mirrors)
        (goto-char (+ offset (overlay-start mirror)))
        (mc/create-fake-cursor-at-point)))
    (mm/clear-all)
    (multiple-cursors-mode)))

(define-key mm/keymap (kbd "C-g") 'mc/switch-from-mark-multiple-to-cursors)

(provide 'mc-mark-multiple-integration)
