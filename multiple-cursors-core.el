(defface mc/cursor-face
  '((t (:inverse-video t)))
  "The face used for fake cursors"
  :group 'multiple-cursors)

(defface mc/region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'multiple-cursors)

(defun mc/make-cursor-overlay-at-eol (pos)
  "Create overlay to look like cursor at end of line."
  (let ((overlay (make-overlay pos pos nil nil t)))
    (overlay-put overlay 'after-string (propertize " " 'face 'mc/cursor-face))
    overlay))

(defun mc/make-cursor-overlay-inline (pos)
  "Create overlay to look like cursor inside text."
  (let ((overlay (make-overlay pos (1+ pos) nil nil t)))
    (overlay-put overlay 'face 'mc/cursor-face)
    overlay))

(defun mc/make-cursor-overlay-at-point ()
  "Create overlay to look like cursor.
Special case for end of line, because overlay over a newline
highlights the entire width of the window."
  (if (eolp)
      (mc/make-cursor-overlay-at-eol (point))
    (mc/make-cursor-overlay-inline (point))))

(defun mc/make-region-overlay-between-point-and-mark ()
  "Create overlay to look like active region."
  (let ((overlay (make-overlay (mark) (point) nil nil t)))
    (overlay-put overlay 'face 'mc/region-face)
    (overlay-put overlay 'type 'additional-region)
    overlay))

(defun mc/store-current-state-in-overlay (o)
  "Store relevant info about point and mark in the given overlay."
  (overlay-put o 'point (set-marker (make-marker) (point)))
  (overlay-put o 'kill-ring kill-ring)
  (overlay-put o 'mark (set-marker (make-marker) (mark)))
  (overlay-put o 'mark-ring mark-ring)
  (overlay-put o 'mark-active mark-active)
  (overlay-put o 'yank-undo-function yank-undo-function)
  (overlay-put o 'kill-ring-yank-pointer kill-ring-yank-pointer)
  (when (boundp 'er/history) (overlay-put o 'er/history er/history))
  o)

(defun mc/restore-state-from-overlay (o)
  "Restore point and mark from stored info in the given overlay."
  (goto-char (overlay-get o 'point))
  (setq kill-ring (overlay-get o 'kill-ring))
  (set-marker (mark-marker) (overlay-get o 'mark))
  (setq mark-ring (overlay-get o 'mark-ring))
  (setq mark-active (overlay-get o 'mark-active))
  (setq yank-undo-function (overlay-get o 'yank-undo-function))
  (setq kill-ring-yank-pointer (overlay-get o 'kill-ring-yank-pointer))
  (when (boundp 'er/history) (setq er/history (overlay-get o 'er/history))))

(defun mc/remove-fake-cursor (o)
  "Delete overlay with state, including dependent overlays and markers."
  (set-marker (overlay-get o 'point) nil)
  (set-marker (overlay-get o 'mark) nil)
  (mc/delete-region-overlay o)
  (delete-overlay o))

(defun mc/pop-state-from-overlay (o)
  "Restore the state stored in given overlay and then remove the overlay."
  (mc/restore-state-from-overlay o)
  (mc/remove-fake-cursor o))

(defun mc/delete-region-overlay (o)
  "Remove the dependent region overlay for a given cursor overlay."
  (ignore-errors
    (delete-overlay (overlay-get o 'region-overlay))))

(defun mc/create-fake-cursor-at-point ()
  "Add a fake cursor and possibly a fake active region overlay based on point and mark.
Saves the current state in the overlay to be restored later."
  (let ((overlay (mc/make-cursor-overlay-at-point)))
    (overlay-put overlay 'type 'additional-cursor)
    (overlay-put overlay 'priority 100)
    (mc/store-current-state-in-overlay overlay)
    (when (use-region-p)
      (overlay-put overlay 'region-overlay
                   (mc/make-region-overlay-between-point-and-mark)))))

(defun mc/execute-command-for-all-fake-cursors (cmd)
  "Calls CMD interactively for each cursor.
It works by moving point to the fake cursor, setting
up the proper kill-ring, and then removing the cursor.
After executing the command, it sets up a new fake
cursor with updated info."
  (let ((annoying-arrows-mode nil))
    (mc/save-excursion
     (mc/for-each-fake-cursor
      (mc/pop-state-from-overlay cursor)
      (ignore-errors
        (call-interactively cmd)
        (when deactivate-mark (deactivate-mark))
        (mc/create-fake-cursor-at-point))))))

(defmacro mc/for-each-fake-cursor (&rest forms)
  "Runs the body for each fake cursor, bound to the name cursor"
  `(mapc #'(lambda (cursor)
             (when (eq (overlay-get cursor 'type) 'additional-cursor)
               ,@forms))
         (overlays-in (point-min) (point-max))))

(defmacro mc/save-excursion (&rest forms)
  "Saves and restores all the state that multiple-cursors cares about."
  `(let ((current-state (mc/store-current-state-in-overlay
                         (make-overlay (point) (point) nil nil t))))
     (overlay-put current-state 'type 'original-cursor)
     (save-excursion ,@forms)
     (mc/pop-state-from-overlay current-state)))

(defun mc/execute-this-command-for-all-cursors ()
  "Used with post-command-hook to execute supported commands for
all cursors. It also checks a list of explicitly unsupported
commands that is prevented even for the original cursor, to
inform about the lack of support.

Commands that are neither supported nor explicitly unsupported
is executed normally for point, but skipped for the fake
cursors."
  (if (memq this-original-command mc--unsupported-cmds)
      (message "%S is not supported with multiple cursors" this-original-command)
    (if (not (memq this-original-command mc--cmds))
        (when (not (memq this-original-command mc--cmds-run-once))
          (message "Skipping %S" this-original-command))
      (mc/execute-command-for-all-fake-cursors this-original-command))))

(defun mc/remove-fake-cursors ()
  "Remove all fake cursors.
Do not use to conclude editing with multiple cursors. For that
you should disable multiple-cursors-mode."
  (mc/for-each-fake-cursor
   (mc/remove-fake-cursor cursor)))

(defun mc/keyboard-quit ()
  "Deactivate mark if there are any active, otherwise exit multiple-cursors-mode."
  (interactive)
  (if (not (use-region-p))
      (multiple-cursors-mode 0)
    (deactivate-mark)))

(defvar mc/keymap nil
  "Keymap while multiple cursors are active.
Main goal of the keymap is to rebind C-g and <return> to conclude
multiple cursors editing.")
(if mc/keymap
    nil
  (setq mc/keymap (make-sparse-keymap))
  (define-key mc/keymap (kbd "C-g") 'mc/keyboard-quit)
  (define-key mc/keymap (kbd "<return>") 'multiple-cursors-mode))

(define-minor-mode multiple-cursors-mode
  "Mode while multiple cursors are active."
  nil " mc" mc/keymap
  (if multiple-cursors-mode
      (add-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t t)
    (remove-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t)
    (mc/remove-fake-cursors)))

(defvar mc--unsupported-cmds '()
  "List of commands that does not work well with multiple cursors.
Set up with the unsupported-cmd macro.")

(defmacro unsupported-cmd (cmd)
  "Adds command to list of unsupported commands and prevents it
from being executed if in multiple-cursors-mode."
  `(progn
     (push (quote ,cmd) mc--unsupported-cmds)
     (defadvice ,cmd (around unsupported-advice activate)
       "command isn't supported with multiple cursors"
       (unless multiple-cursors-mode
         ad-do-it))))

;; Commands that does not work with multiple-cursors
(unsupported-cmd isearch-forward)
(unsupported-cmd isearch-backward)

;; Commands to run only once (not yet in use)
(setq mc--cmds-run-once '(mark-next-like-this
                          save-buffer
                          undo
                          undo-tree-undo
                          undo-tree-redo))

;; Commands that should be mirrored by all cursors
(setq mc--cmds '(mc/keyboard-quit
                 self-insert-command
                 js2-insert-and-indent
                 wrap-region-trigger
                 sgml-slash
                 slime-space
                 previous-line
                 next-line
                 newline
                 yas/expand
                 newline-and-indent
                 join-line
                 right-char forward-char
                 right-word forward-word
                 left-char backward-char
                 left-word backward-word
                 subword-upcase upcase-word
                 subword-downcase downcase-word
                 subword-capitalize capitalize-word
                 forward-list
                 backward-list
                 hippie-expand hippie-expand-lines
                 yank yank-indented
                 kill-word
                 kill-region-or-backward-word
                 kill-line
                 kill-whole-line
                 backward-kill-word
                 backward-delete-char-untabify
                 delete-char c-electric-delete-forward
                 delete-backward-char c-electric-backspace
                 c-electric-paren
                 c-electric-semi&comma
                 org-shiftright
                 just-one-space
                 zap-to-char
                 end-of-line
                 set-mark-command
                 js2-beginning-of-line
                 js2-end-of-line
                 js2r-inline-var
                 change-number-at-point
                 move-end-of-line
                 move-end-of-line-or-next-line
                 beginning-of-line
                 er/expand-region
                 er/mark-word
                 smart-forward
                 smart-backward
                 smart-up
                 smart-down
                 move-beginning-of-line
                 move-start-of-line-or-prev-line
                 dired-back-to-start-of-files
                 save-region-or-current-line
                 back-to-indentation))

(provide 'multiple-cursors-core)
