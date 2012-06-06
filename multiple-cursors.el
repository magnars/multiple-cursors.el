;;; multiple-cursors.el --- An experiment in multiple cursors for emacs.

;; Copyright (C) 2012 Magnar Sveen

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

;; This extension is dependent on the mark-multiple library.
;;     https://github.com/magnars/mark-multiple.el

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
;; out, and also working on combining it with
;; [mark-multiple.el](https://github.com/magnars/mark-multiple.el) to get a more
;; comprehensive tool.
;;
;; Still, if you've got something to contribute, please do not hesitate to open
;; an issue, and we can take a look together before you dive into the elisp. :-)
;;
;; You'll find the repo at:
;;
;;     https://github.com/magnars/multiple-cursors.el

;; ## Combining with mark-multiple
;;
;; Right now you can go from multiple marks to multiple cursors with C-g.
;;
;; The other way around is a bit more tricky:
;;
;;  * What to do about overlapping marks?
;;  * Expanding the marks should be possible, for instance using `mark-word` or
;;    `expand-region`
;;  * Killing or copying needs to keep a kill-ring for each cursor.
;;
;; So basically `mark-multiple` isn't ready for prime time as a full blown multiple
;; marks library. For this to work as expected, I think parts of mark-multiple
;; needs to be rewritten, and possibly integrated into multiple-cursors.
;;
;; For now, mark-multiple is an excellent tool to place your cursors where you need
;; them to be.

;;; Code:

(require 'mark-multiple)

(defface mc/cursor-face
  '((t (:inverse-video t)))
  "The face used for additional cursors"
  :group 'multiple-cursors)

(defface mc/region-face
  '((t :inherit region))
  "The face used for additional regions"
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
  (let ((overlay (make-overlay (mark) (point) nil nil t)))
    (overlay-put overlay 'face 'mc/region-face)
    (overlay-put overlay 'type 'additional-region)
    overlay))

;; TODO:
;; (set-marker MARKER nil) for performance
;; collapse cursors at same point
;; remove mark-multiple integration
;; C-g fjerner regions først, før den disabler multiple-cursors
;; refactor and add tests :-P

;; ALSO:
;; unknown:
;;    (t)ry all or (i)gnore -> (did that work ok? (k)eep doing that or (d)on't)

(defun mc/add-cursor-at-point ()
  "Add a fake cursor where point is.
Also makes a copy of the kill-ring to be used by this cursor."
  (let ((overlay (mc/make-cursor-overlay-at-point)))
    (overlay-put overlay 'type 'additional-cursor)
    (overlay-put overlay 'kill-ring kill-ring)
    (overlay-put overlay 'mark-ring mark-ring)
    (overlay-put overlay 'mark-active mark-active)
    (overlay-put overlay 'mark (set-marker (make-marker) (mark)))
    (when (use-region-p)
      (overlay-put overlay 'region-overlay
                   (mc/make-region-overlay-between-point-and-mark)))
    (overlay-put overlay 'priority 100)))

(defun mc/execute-command-for-all-fake-cursors (cmd)
  "Calls CMD interactively for each cursor.
It works by moving point to the fake cursor, setting
up the proper kill-ring, and then removing the cursor.
After executing the command, it sets up a new fake
cursor with updated info."
  (let ((current-kill-ring kill-ring)
        (current-mark-ring mark-ring)
        (current-mark-active mark-active)
        (annoying-arrows-mode nil))
    (save-excursion
      (mapc #'(lambda (o)
                (when (eq (overlay-get o 'type) 'additional-cursor)
                  (goto-char (overlay-start o))
                  (setq kill-ring (overlay-get o 'kill-ring))
                  (set-marker (mark-marker) (overlay-get o 'mark))
                  (setq mark-ring (overlay-get o 'mark-ring))
                  (setq mark-active (overlay-get o 'mark-active))
                  (delete-region-overlay o)
                  (delete-overlay o)
                  (ignore-errors
                    (call-interactively cmd)
                    (when deactivate-mark (deactivate-mark))
                    (mc/add-cursor-at-point))))
            (overlays-in (point-min) (point-max))))
    (setq kill-ring current-kill-ring)
    (setq mark-ring current-mark-ring)
    (setq mark-active current-mark-active)))

(defun delete-region-overlay (o)
  (ignore-errors
    (delete-overlay (overlay-get o 'region-overlay))))

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
      (mc/execute-command-for-all-fake-cursors this-original-command))))

(defun mc/remove-additional-cursors ()
  "Remove all fake cursors.
Do not use to conclude editing with multiple cursors. For that
you should disable multiple-cursors-mode."
  (mapc #'(lambda (o)
            (when (eq (overlay-get o 'type) 'additional-cursor)
              (delete-region-overlay o)
              (delete-overlay o)))
        (overlays-in (point-min) (point-max))))

(defvar mc/keymap nil
  "Keymap while multiple cursors are active.
Main goal of the keymap is to rebind C-g and <return> to conclude
multiple cursors editing.")
(if mc/keymap
    nil
  (setq mc/keymap (make-sparse-keymap))
  (define-key mc/keymap (kbd "C-g") 'multiple-cursors-mode)
  (define-key mc/keymap (kbd "<return>") 'multiple-cursors-mode))

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
  (mc/execute-command-for-all-fake-cursors 'end-of-line)
  (end-of-line))

(defun mc/edit-beginnings-of-lines ()
  "Add one cursor to the beginning of each line in the active region."
  (interactive)
  (mc/add-multiple-cursors-to-region-lines)
  (mc/execute-command-for-all-fake-cursors 'beginning-of-line)
  (beginning-of-line))

(defun mc/switch-from-mark-multiple-to-cursors ()
  "Removes mark-multiple and switches to multiple cursors instead"
  (interactive)
  (let ((offset (- (point) (overlay-start mm/master))))
    (deactivate-mark)
    (save-excursion
      (dolist (mirror mm/mirrors)
        (goto-char (+ offset (overlay-start mirror)))
        (mc/add-cursor-at-point)))
    (mm/clear-all)
    (multiple-cursors-mode)))

(define-key mm/keymap (kbd "C-g") 'mc/switch-from-mark-multiple-to-cursors)

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

;; Commands that make a giant mess of multiple cursors
(unsupported-cmd yank-pop)

;; Commands that should be mirrored by all cursors
(setq mc--cmds '(self-insert-command
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
                 move-beginning-of-line
                 move-start-of-line-or-prev-line
                 dired-back-to-start-of-files
                 save-region-or-current-line
                 back-to-indentation))

(provide 'multiple-cursors)

;;; multiple-cursors.el ends here
