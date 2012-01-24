(defface mc/cursor-face
  '((t (:inverse-video t)))
  "The face used for additional cursors"
  :group 'multiple-cursors)

(defun mc/make-cursor-overlay-at-eol (pos)
  (let ((overlay (make-overlay pos pos nil nil t)))
    (overlay-put overlay 'after-string (propertize " " 'face 'mc/cursor-face))
    overlay))

(defun mc/make-cursor-overlay-inline (pos)
  (let ((overlay (make-overlay pos (1+ pos) nil nil t)))
    (overlay-put overlay 'face 'mc/cursor-face)
    overlay))

(defun mc/make-cursor-overlay-at-point ()
  (if (eolp)
      (mc/make-cursor-overlay-at-eol (point))
    (mc/make-cursor-overlay-inline (point))))

(defun mc/add-cursor-at-point ()
  (let ((overlay (mc/make-cursor-overlay-at-point)))
    (overlay-put overlay 'type 'additional-cursor)
    (overlay-put overlay 'priority 100)))

(setq mc--cmds '(self-insert-command
                 previous-line
                 next-line
                 newline
                 right-char
                 right-word
                 left-char
                 left-word
                 yank
                 kill-word
                 kill-region-or-backward-word
                 backward-kill-word
                 backward-delete-char-untabify
                 delete-char
                 delete-backward-char
                 move-end-of-line-or-next-line
                 move-start-of-line-or-prev-line))

(defun mc/execute-this-command-for-all-cursors ()
  (if (not (memq this-original-command mc--cmds))
      (message "Skipping %S" this-original-command)
    (save-excursion
      (mapc #'(lambda (o)
                (when (eq (overlay-get o 'type) 'additional-cursor)
                  (goto-char (overlay-start o))
                  (delete-overlay o)
                  (ignore-errors
                    (call-interactively this-original-command)
                    (mc/add-cursor-at-point))))
            (overlays-in (point-min) (point-max))))))

(defun mc/remove-additional-cursors ()
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'additional-cursor)
              (delete-overlay o)))
        (overlays-in (point-min) (point-max))))

(defvar mc/keymap nil
  "Keymap while multiple cursors are active.")
(if mc/keymap
    nil
  (setq mc/keymap (make-sparse-keymap))
  (define-key mc/keymap (kbd "C-g") 'multiple-cursors-mode))

(define-minor-mode multiple-cursors-mode
  "Mode while multiple cursors are active."
  nil " mc" mc/keymap
  (cond ((not multiple-cursors-mode)
         (remove-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t)
         (mc/remove-additional-cursors))
        (t (add-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t t))))

(defun mc/edit-ends-of-lines ()
  (interactive)
  (mc/remove-additional-cursors)
  (let* ((point-line (line-number-at-pos))
         (mark-line (save-excursion (exchange-point-and-mark) (line-number-at-pos)))
         (num-cursors (abs (- point-line mark-line)))
         (navigation-func (if (< point-line mark-line) 'previous-line 'next-line)))
    (exchange-point-and-mark)
    (while (not (eq (line-number-at-pos) point-line))
      (end-of-line)
      (mc/add-cursor-at-point)
      (funcall navigation-func))
    (end-of-line)
    (deactivate-mark)
    (multiple-cursors-mode)))

(provide 'multiple-cursors)
