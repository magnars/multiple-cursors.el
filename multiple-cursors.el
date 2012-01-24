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
    (overlay-put overlay 'kill-ring kill-ring)
    (overlay-put overlay 'priority 100)))

(setq mc--cmds '(self-insert-command
                 previous-line
                 next-line
                 newline
                 right-char forward-char
                 right-word forward-word
                 left-char backward-char
                 left-word backward-word
                 yank
                 kill-word
                 kill-region-or-backward-word
                 backward-kill-word
                 backward-delete-char-untabify
                 delete-char
                 delete-backward-char
                 move-end-of-line-or-next-line
                 move-start-of-line-or-prev-line))

(setq mc--unsupported-cmds '(yank-pop))

;; todo: macro-ify and iterate over mc--unsupported-cmds
(defadvice yank-pop (around yank-pop-unsupported-advice activate)
  "yank-pop isn't supported with multiple cursors"
  (unless multiple-cursors-mode
    ad-do-it))

(defun mc/execute-command-for-all-cursors (cmd)
  (let ((current-kill-ring kill-ring))
    (save-excursion
      (mapc #'(lambda (o)
                (when (eq (overlay-get o 'type) 'additional-cursor)
                  (goto-char (overlay-start o))
                  (setq kill-ring (overlay-get o 'kill-ring))
                  (delete-overlay o)
                  (ignore-errors
                    (call-interactively cmd)
                    (mc/add-cursor-at-point))))
            (overlays-in (point-min) (point-max))))
    (setq kill-ring current-kill-ring)))

(defun mc/execute-this-command-for-all-cursors ()
  (if (memq this-original-command mc--unsupported-cmds)
      (message "%S is not supported with multiple cursors" this-original-command)
    (if (not (memq this-original-command mc--cmds))
        (message "Skipping %S" this-original-command)
      (mc/execute-command-for-all-cursors this-original-command))))

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

(defun mc/add-multiple-cursors-to-region-lines ()
  (interactive)
  (when (not (use-region-p))
    (error "Mark a set of lines first."))
  (mc/remove-additional-cursors)
  (let* ((point-line (line-number-at-pos))
         (mark-line (save-excursion (exchange-point-and-mark) (line-number-at-pos)))
         (num-cursors (abs (- point-line mark-line)))
         (navigation-func (if (< point-line mark-line) 'previous-line 'next-line)))
    (exchange-point-and-mark)
    (while (not (eq (line-number-at-pos) point-line))
      (mc/add-cursor-at-point)
      (funcall navigation-func))
    (deactivate-mark)
    (multiple-cursors-mode)))

(defun mc/edit-ends-of-lines ()
  (interactive)
  (mc/add-multiple-cursors-to-region-lines)
  (mc/execute-command-for-all-cursors 'end-of-line)
  (end-of-line))

(defun mc/edit-beginnings-of-lines ()
  (interactive)
  (mc/add-multiple-cursors-to-region-lines)
  (mc/execute-command-for-all-cursors 'beginning-of-line)
  (beginning-of-line))

(provide 'multiple-cursors)
