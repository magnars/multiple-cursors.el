;;; multiple-cursors-core.el --- An experiment in multiple cursors for emacs.

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

;; This file contains the core functionality of multiple-cursors.
;; Please see multiple-cursors.el for more commentary.

;;; Code:

(eval-when-compile (require 'cl))

(defface mc/cursor-face
  '((t (:inverse-video t)))
  "The face used for fake cursors"
  :group 'multiple-cursors)

(defface mc/region-face
  '((t :inherit region))
  "The face used for fake regions"
  :group 'multiple-cursors)

(defmacro mc/add-fake-cursor-to-undo-list (&rest forms)
  "Make sure point is in the right place when undoing"
  `(let ((undo-cleaner (cons 'apply (cons 'deactivate-cursor-after-undo (list id)))))
     (setq buffer-undo-list (cons undo-cleaner buffer-undo-list))
     ,@forms
     (if (eq undo-cleaner (car buffer-undo-list)) ;; if nothing has been added to the undo-list
         (setq buffer-undo-list (cdr buffer-undo-list)) ;; then pop the cleaner right off again
       (setq buffer-undo-list ;; otherwise add a function to activate this cursor
             (cons (cons 'apply (cons 'activate-cursor-for-undo (list id))) buffer-undo-list)))))

(defmacro mc/for-each-fake-cursor (&rest forms)
  "Runs the body for each fake cursor, bound to the name cursor"
  `(mapc #'(lambda (cursor)
             (when (mc/fake-cursor-p cursor)
               ,@forms))
         (overlays-in (point-min) (point-max))))

(defmacro mc/save-excursion (&rest forms)
  "Saves and restores all the state that multiple-cursors cares about."
  `(let ((current-state (mc/store-current-state-in-overlay
                         (make-overlay (point) (point) nil nil t))))
     (overlay-put current-state 'type 'original-cursor)
     (save-excursion ,@forms)
     (mc/pop-state-from-overlay current-state)))

(defmacro mc/save-window-scroll (&rest forms)
  "Saves and restores the window scroll position"
  `(let ((p (set-marker (make-marker) (point)))
         (start (set-marker (make-marker) (window-start)))
         (hscroll (window-hscroll)))
     ,@forms
     (goto-char p)
     (set-window-start nil start)
     (set-window-hscroll nil hscroll)
     (set-marker p nil)
     (set-marker start nil)))

(defun mc/make-cursor-overlay-at-eol (pos)
  "Create overlay to look like cursor at end of line."
  (let ((overlay (make-overlay pos pos nil nil nil)))
    (overlay-put overlay 'after-string (propertize " " 'face 'mc/cursor-face))
    overlay))

(defun mc/make-cursor-overlay-inline (pos)
  "Create overlay to look like cursor inside text."
  (let ((overlay (make-overlay pos (1+ pos) nil nil nil)))
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
  (overlay-put o 'kill-ring-yank-pointer kill-ring-yank-pointer)
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
  (setq kill-ring-yank-pointer (overlay-get o 'kill-ring-yank-pointer))
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

(defvar mc--current-cursor-id 0
  "Var to store increasing id of fake cursors, used to keep track of them for undo.")

(defun mc/create-cursor-id ()
  "Returns a unique cursor id"
  (incf mc--current-cursor-id))

(defun mc/create-fake-cursor-at-point (&optional id)
  "Add a fake cursor and possibly a fake active region overlay based on point and mark.
Saves the current state in the overlay to be restored later."
  (let ((overlay (mc/make-cursor-overlay-at-point)))
    (overlay-put overlay 'mc-id (or id (mc/create-cursor-id)))
    (overlay-put overlay 'type 'fake-cursor)
    (overlay-put overlay 'priority 100)
    (mc/store-current-state-in-overlay overlay)
    (when (use-region-p)
      (overlay-put overlay 'region-overlay
                   (mc/make-region-overlay-between-point-and-mark)))))

(defun mc/execute-command (cmd)
  "Run command, simulating the parts of the command loop that makes sense for fake cursors."
  (setq this-command cmd)
  (run-hooks 'pre-command-hook)
  (unless (eq this-command 'ignore)
    (call-interactively cmd))
  (when deactivate-mark (deactivate-mark)))

(defun mc/execute-command-for-all-fake-cursors (cmd)
  "Calls CMD interactively for each cursor.
It works by moving point to the fake cursor, setting
up the proper environment, and then removing the cursor.
After executing the command, it sets up a new fake
cursor with updated info."
  (mc/save-excursion
   (mc/save-window-scroll
    (mc/for-each-fake-cursor
     (save-excursion
       (let ((id (overlay-get cursor 'mc-id))
             (annoying-arrows-mode nil))
         (mc/add-fake-cursor-to-undo-list
          (mc/pop-state-from-overlay cursor)
          (ignore-errors
            (mc/execute-command cmd)
            (mc/create-fake-cursor-at-point id)))))))))

(defun mc/fake-cursor-p (o)
  "Predicate to check if an overlay is a fake cursor"
  (eq (overlay-get o 'type) 'fake-cursor))

(defun mc/cursor-with-id (id)
  "Find the first cursor with the given id, or nil"
  (find-if #'(lambda (o) (and (mc/fake-cursor-p o)
                              (= id (overlay-get o 'mc-id))))
           (overlays-in (point-min) (point-max))))

(defvar mc--stored-state-for-undo nil
  "Variable to keep the state of the real cursor while undoing a fake one")

(defun activate-cursor-for-undo (id)
  "Called when undoing to temporarily activate the fake cursor which action is being undone."
  (let ((cursor (mc/cursor-with-id id)))
    (when cursor
      (setq mc--stored-state-for-undo (mc/store-current-state-in-overlay
                                       (make-overlay (point) (point) nil nil t)))
      (mc/pop-state-from-overlay cursor))))

(defun deactivate-cursor-after-undo (id)
  "Called when undoing to reinstate the real cursor after undoing a fake one."
  (when mc--stored-state-for-undo
    (mc/create-fake-cursor-at-point id)
    (mc/pop-state-from-overlay mc--stored-state-for-undo)
    (setq mc--stored-state-for-undo nil)))

(defun mc/prompt-for-inclusion-in-whitelist (original-command)
  "Asks the user, then adds the command either to the once-list or the all-list."
  (let ((all-p (y-or-n-p (format "Do %S for all cursors?" original-command))))
    (if all-p
        (add-to-list 'mc/cmds-to-run-for-all original-command)
      (add-to-list 'mc/cmds-to-run-once original-command))
    (mc/save-lists)
    all-p))

(defun mc/num-cursors ()
  "The number of cursors (real and fake) in the buffer."
  (1+ (count-if 'mc/fake-cursor-p
                (overlays-in (point-min) (point-max)))))

(defun mc/execute-this-command-for-all-cursors ()
  "Used with post-command-hook to execute supported commands for
all cursors. It also checks a list of explicitly unsupported
commands that is prevented even for the original cursor, to
inform about the lack of support.

Commands that are neither supported nor explicitly unsupported
is executed normally for point, but skipped for the fake
cursors."
  (if (eq 1 (mc/num-cursors)) ;; no fake cursors? disable mc-mode
      (multiple-cursors-mode 0)
    (let ((original-command (or (command-remapping this-original-command)
                                this-original-command)))

      ;; if it's a lambda, we can't know if it's supported or not
      ;; - so go ahead and assume it's ok, because we're just optimistic like that
      (if (not (symbolp original-command))
          (mc/execute-command-for-all-fake-cursors original-command)

        ;; otherwise it's a symbol, and we can be more thorough
        (if (get original-command 'mc--unsupported)
            (message "%S is not supported with multiple cursors%s"
                     original-command
                     (get original-command 'mc--unsupported))
          (when (and original-command
                     (not (memq original-command mc--default-cmds-to-run-once))
                     (not (memq original-command mc/cmds-to-run-once))
                     (or (memq original-command mc--default-cmds-to-run-for-all)
                         (memq original-command mc/cmds-to-run-for-all)
                         (mc/prompt-for-inclusion-in-whitelist original-command)))
            (mc/execute-command-for-all-fake-cursors original-command)))))))

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
      (progn
        (add-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t t)
        (run-hooks 'multiple-cursors-mode-enabled-hook))
    (remove-hook 'post-command-hook 'mc/execute-this-command-for-all-cursors t)
    (mc/remove-fake-cursors)
    (run-hooks 'multiple-cursors-mode-disabled-hook)))

(add-hook 'after-revert-hook #'(lambda () (multiple-cursors-mode 0)))

(defmacro unsupported-cmd (cmd msg)
  "Adds command to list of unsupported commands and prevents it
from being executed if in multiple-cursors-mode."
  `(progn
     (put (quote ,cmd) 'mc--unsupported ,msg)
     (defadvice ,cmd (around unsupported-advice activate)
       "command isn't supported with multiple cursors"
       (unless (and multiple-cursors-mode (called-interactively-p 'any))
         ad-do-it))))

;; Commands that does not work with multiple-cursors
(unsupported-cmd isearch-forward ". Feel free to add a compatible version.")
(unsupported-cmd isearch-backward ". Feel free to add a compatible version.")

;; Make sure pastes from other programs are added to all kill-rings when yanking
(defadvice current-kill (before interprogram-paste-for-all-cursors activate)
  (let ((interprogram-paste (and (= n 0)
                                 interprogram-paste-function
                                 (funcall interprogram-paste-function))))
    (when interprogram-paste
      ;; Add interprogram-paste to normal kill ring, just
      ;; like current-kill usually does for itself.
      ;; We have to do the work for it tho, since the funcall only returns
      ;; something once. It is not a pure function.
      (let ((interprogram-cut-function nil))
        (if (listp interprogram-paste)
            (mapc 'kill-new (nreverse interprogram-paste))
          (kill-new interprogram-paste))
        ;; And then add interprogram-paste to the kill-rings
        ;; of all the other cursors too.
        (mc/for-each-fake-cursor
         (let ((kill-ring (overlay-get cursor 'kill-ring))
               (kill-ring-yank-pointer (overlay-get cursor 'kill-ring-yank-pointer)))
           (if (listp interprogram-paste)
               (mapc 'kill-new (nreverse interprogram-paste))
             (kill-new interprogram-paste))
           (overlay-put cursor 'kill-ring kill-ring)
           (overlay-put cursor 'kill-ring-yank-pointer kill-ring-yank-pointer)))))))

(defvar mc/list-file "~/.emacs.d/.mc-lists.el"
  "The position of the file that keeps track of your preferences
for running commands with multiple cursors.")

(defun mc/save-lists ()
  (with-temp-file mc/list-file
    (emacs-lisp-mode)
    (insert ";; This file is automatically generated by the multiple-cursors extension.")
    (newline)
    (insert ";; It keeps track of your preferences for running commands with multiple cursors.")
    (newline)
    (newline)
    (insert "(setq mc/cmds-to-run-for-all '(")
    (mapc #'(lambda (cmd) (insert (format "%S" cmd)) (newline-and-indent)) mc/cmds-to-run-for-all)
    (when mc/cmds-to-run-for-all
      (next-line -1)
      (end-of-line))
    (insert "))")
    (newline)
    (newline)
    (insert "(setq mc/cmds-to-run-once '(")
    (mapc #'(lambda (cmd) (insert (format "%S" cmd)) (newline-and-indent)) mc/cmds-to-run-once)
    (when mc/cmds-to-run-once
      (next-line -1)
      (end-of-line))
    (insert "))")
    (newline)))

(defvar mc/cmds-to-run-once nil
  "Commands to run only once in multiple-cursors-mode.")

(defvar mc--default-cmds-to-run-once nil
  "Default set of commands to run only once in multiple-cursors-mode.")

(setq mc--default-cmds-to-run-once '(mc/switch-from-mark-multiple-to-cursors
                                     mc/edit-lines
                                     mc/edit-ends-of-lines
                                     mc/edit-beginnings-of-lines
                                     mc/mark-next-like-this
                                     mc/mark-previous-like-this
                                     mc/mark-more-like-this-extended
                                     mc/mark-all-like-this
                                     mc/cycle-forward
                                     mc/cycle-backward
                                     rrm/switch-to-multiple-cursors
                                     save-buffer
                                     ido-exit-minibuffer
                                     exit-minibuffer
                                     undo
                                     redo
                                     undo-tree-undo
                                     undo-tree-redo
                                     universal-argument
                                     universal-argument-more
                                     universal-argument-other-key
                                     negative-argument
                                     digit-argument
                                     top-level
                                     recenter-top-bottom
                                     describe-mode
                                     describe-key-1
                                     describe-function
                                     describe-bindings
                                     describe-prefix-bindings
                                     other-window
                                     kill-buffer-and-window
                                     split-window-right
                                     split-window-below
                                     delete-other-windows
                                     toggle-window-split
                                     windmove-left
                                     windmove-right
                                     windmove-up
                                     windmove-down))

(defvar mc--default-cmds-to-run-for-all nil
  "Default set of commands that should be mirrored by all cursors")

(setq mc--default-cmds-to-run-for-all '(mc/keyboard-quit
                                        self-insert-command
                                        previous-line
                                        next-line
                                        newline
                                        newline-and-indent
                                        open-line
                                        transpose-chars
                                        transpose-lines
                                        transpose-paragraphs
                                        transpose-regions
                                        join-line
                                        right-char
                                        right-word
                                        forward-char
                                        forward-word
                                        left-char
                                        left-word
                                        backward-char
                                        backward-word
                                        upcase-word
                                        downcase-word
                                        capitalize-word
                                        forward-list
                                        backward-list
                                        hippie-expand
                                        hippie-expand-lines
                                        yank
                                        yank-pop
                                        kill-word
                                        kill-line
                                        kill-whole-line
                                        backward-kill-word
                                        backward-delete-char-untabify
                                        delete-char delete-forward-char
                                        delete-backward-char
                                        just-one-space
                                        zap-to-char
                                        end-of-line
                                        set-mark-command
                                        move-end-of-line
                                        beginning-of-line
                                        move-beginning-of-line
                                        kill-ring-save
                                        back-to-indentation
                                        subword-forward
                                        subword-backward
                                        subword-mark
                                        subword-kill
                                        subword-backward-kill
                                        subword-transpose
                                        subword-capitalize
                                        subword-upcase
                                        subword-downcase
                                        er/expand-region
                                        er/contract-region
                                        smart-forward
                                        smart-backward
                                        smart-up
                                        smart-down))

(defvar mc/cmds-to-run-for-all nil
  "Commands to run for all cursors in multiple-cursors-mode")

(load mc/list-file t) ;; load, but no errors if it does not exist yet please

(provide 'multiple-cursors-core)

;;; multiple-cursors-core.el ends here
