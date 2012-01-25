;;; multiple-cursors.el --- An experiment in multiple cursors for emacs.

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: editing cursors

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; An experiment in multiple cursors for emacs. Still very much in beta.
;;
;; The basic concept works, but there are definitely some kinks to work out.

;; ** Usage

;; I've set up my key-bindings like so:
;;
;;     ;; Experimental multiple-cursors
;;     (global-set-key (kbd "C-S-c C-S-c") 'mc/add-multiple-cursors-to-region-lines)
;;     (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
;;     (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
;;
;; To get out of multiple-cursors-mode, press `C-g`.

;; ** Contribute

;; There's plenty wrong with this implementation still. I'm actively trying things
;; out, and also considering combining it with
;; [mark-multiple.el](https://github.com/magnars/mark-multiple.el) to get a more
;; comprehensive tool.
;;
;; Still, if you've got something to contribute, please do not hesitate to open
;; an issue, and we can take a look together before you dive into the elisp. :-)
;;
;; You'll find the repo at:
;;
;;     https://github.com/magnars/multiple-cursors.el

;;; Code:

(defface mc/cursor-face
  '((t (:inverse-video t)))
  "The face used for additional cursors"
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

(defun mc/add-cursor-at-point ()
  "Add a fake cursor where point is.
Also makes a copy of the kill-ring to be used by this cursor."
  (let ((overlay (mc/make-cursor-overlay-at-point)))
    (overlay-put overlay 'type 'additional-cursor)
    (overlay-put overlay 'kill-ring kill-ring)
    (overlay-put overlay 'priority 100)))

(defvar mc--unsupported-cmds '()
  "List of commands that does not work well with multiple cursors.")

(defmacro unsupported-cmd (cmd)
  "Adds command to list of unsupported commands and prevents it
from being executed if in multiple-cursors-mode."
  `(progn
     (push (quote ,cmd) mc--unsupported-cmds)
     (defadvice ,cmd (around unsupported-advice activate)
       "command isn't supported with multiple cursors"
       (unless multiple-cursors-mode
         ad-do-it))))

;; Commands that makes a giant mess of multiple cursors
(unsupported-cmd yank-pop)

;; Commands that should be mirrored by all cursors
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
                 zap-to-char
                 move-end-of-line-or-next-line
                 move-start-of-line-or-prev-line))

(defun mc/execute-command-for-all-cursors (cmd)
  "Calls CMD interactively for each cursor.
It works by moving point to the fake cursor, setting
up the proper kill-ring, and then removing the cursor.
After executing the command, it sets up a new fake
cursor with updated info."
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
        (message "Skipping %S" this-original-command)
      (mc/execute-command-for-all-cursors this-original-command))))

(defun mc/remove-additional-cursors ()
  "Remove all fake cursors.
Do not use to conclude editing with multiple cursors. For that
you should disable multiple-cursors-mode."
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'additional-cursor)
              (delete-overlay o)))
        (overlays-in (point-min) (point-max))))

(defvar mc/keymap nil
  "Keymap while multiple cursors are active.
Main goal of the keymap is to rebind C-g to conclude multiple
cursors editing.")
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
  "Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on.

Could possibly be used to mark multiple regions with
mark-multiple if point and mark is on different columns."
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
  "Add one cursor to the end of each line in the active region."
  (interactive)
  (mc/add-multiple-cursors-to-region-lines)
  (mc/execute-command-for-all-cursors 'end-of-line)
  (end-of-line))

(defun mc/edit-beginnings-of-lines ()
  "Add one cursor to the beginning of each line in the active region."
  (interactive)
  (mc/add-multiple-cursors-to-region-lines)
  (mc/execute-command-for-all-cursors 'beginning-of-line)
  (beginning-of-line))

(provide 'multiple-cursors)

;;; multiple-cursors.el ends here
