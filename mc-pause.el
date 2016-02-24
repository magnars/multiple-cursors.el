;;; mc-pause.el --- Pause the execution of commands for fake cursors.

(require 'multiple-cursors-core)

(defvar mc/paused-mode nil)
(defvar mc/paused-mode-map (make-sparse-keymap))

(make-variable-buffer-local
 (defvar mc--paused-point-cursor-overlay nil))

(make-variable-buffer-local
 (defvar mc--pause-reenabled-minor-modes nil))

(defcustom mc/unpause-jump-back t
  "If we want to jump back when unpause."
  :type 'boolean
  :group 'multiple-cursors)

(defcustom mc/pause-reenable-minor-modes nil
  "Minor modes that were disabled my multiple-cursors and
you wnat to reenable during the pause."
  :type '(repeat symbol)
  :group 'multiple-cursors)


(setq mc--default-cmds-to-run-once
      (append '(mc/pause mc/unpause mc/toggle-pause)
              mc--default-cmds-to-run-once))


;;;###autoload
(defun mc/pause (&optional do-not-remember-point)
  (interactive "P")
  (unless mc/paused-mode
    (mc/paused-mode 1)
    (unless do-not-remember-point
      (setq mc--paused-point-cursor-overlay (mc/create-fake-cursor-at-point)))))

;;;###autoload
(defun mc/unpause (&optional do-not-restore-point)
  (interactive "P")
  (when mc/paused-mode
    (when (and (not do-not-restore-point)
               mc/unpause-jump-back
               mc--paused-point-cursor-overlay)
      (mc/pop-state-from-overlay mc--paused-point-cursor-overlay))
    (mc/paused-mode -1)))

;;;###autoload
(defun mc/toggle-pause (&optional p)
  (interactive "P")
  (if mc/paused-mode
      (mc/unpause p)
    (mc/pause p)))


(defun mc/--pause-reenable-minor-modes ()
  (mapc #'(lambda (mode)
            (when (and (boundp mode) (not (eval mode))
                       (memq mode mc/temporarily-disabled-minor-modes)
                       (funcall mode 1))
              (push mode mc--pause-reenabled-minor-modes)))
        mc/pause-reenable-minor-modes))

(defun mc/--pause-redisable-minor-modes ()
  (mapc #'(lambda (mode)
            (when (and (boundp mode) (eval mode))
              (funcall mode -1)))
        mc--pause-reenabled-minor-modes)
  (setq mc--pause-reenabled-minor-modes nil))

(defun mc/--pause-mode-switch-off ()
  (when mc/paused-mode
    (mc/paused-mode -1)))

;;;###autoload
(define-minor-mode mc/paused-mode
  "Mode while multiple cursors are paused."
  nil
  (" mc:" (:eval (propertize "paused" 'face 'font-lock-warning-face)))
  mc/paused-mode-map
  (setq mc--paused-point-cursor-overlay nil)
  (if mc/paused-mode
      (if multiple-cursors-mode
          (progn
            (remove-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t)
            (remove-hook 'pre-command-hook 'mc/make-a-note-of-the-command-being-run t)
            (add-hook 'multiple-cursors-mode-disabled-hook #'mc/--pause-mode-switch-off nil t)
            (mc/--pause-reenable-minor-modes))
        (setq mc/paused-mode nil))
    (mc/--pause-redisable-minor-modes)
    (add-hook 'pre-command-hook 'mc/make-a-note-of-the-command-being-run nil t)
    (add-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t t)
    (remove-hook 'multiple-cursors-mode-disabled-hook #'mc/--pause-mode-switch-off t)))


(provide 'mc-pause)

;;; mc-pause.el ends here
