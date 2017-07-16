;;; mc-evil.el --- An experiment in multiple cursors for emacs.

;; Copyright (C) 2012-2016 Magnar Sveen
;; Copyright (C) 2015 Martin Yrjölä

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

;; This file contains the functionality of multiple-cursors relative
;; to Evil integration.  Please see multiple-cursors.el for more
;; commentary.

;;; Code:

(require 'mc-vars)

(defun mc/evil-p ()
  "t if we are using evil, nil otherwise."
  (and (featurep 'evil) evil-mode))

(defun mc/evil-maybe-visual-refresh ()
  "Call `evil-visual-refresh' if
- evil mode is being used
- in evil visual state
- the command is a motion
- the command is a text-object motion that operates on an active region (evil interactive code `<v>'
- point and mark don't line up with the current `evil-visual-*' variables"
  (when (and (mc/evil-p)
             (eq 'motion (evil-get-command-property this-command :repeat))
             (evil-get-command-property this-command :keep-visual))
    (evil-visual-refresh (mark) (point))))

(defun mc/overlay-change-evil-state-p (o)
  "Is the current evil-state variable equal to the state stored
in the fake cursor `O'."
  (not (eq evil-state (overlay-get o 'evil-state))))

(defun mc/restore-overlay-vars (o vars)
  "Restore fake cursor `O' state variables in list `VARS'"
  (dolist (var vars)
    (when (boundp var) (set var (overlay-get o var)))))

(defun mc/evil-fake-cursor-state-transition (o)
  "Transition from current `evil-state' to the fake cursor `O'
`evil-state'. `evil-state' does not need to be restored as part
of `mc/cursor-specific-vars'.  There are a few special cases to handle for the following state transitions
- `insert' -> `normal'
- `replace' -> `normal'
- -> `visual' and fake cursor `O' has property `evil-visual-contracted'
- `normal' -> `insert'
- `insert' -> `visual' and `evil-visual-selection' is `line'.
Otherwise, just transition to fake cursors `evil-state'"
  (let ((overlay-evil-state (overlay-get o 'evil-state))
        (cursor-specific-vars (cl-remove-if #'(lambda (var) (eq var 'evil-state)) mc/cursor-specific-vars)))
    (mc/restore-overlay-vars o cursor-specific-vars)
    (cond
     ((or (and (eq evil-state 'insert)
               (eq overlay-evil-state 'normal))
          (and (eq evil-state 'replace)
               (eq overlay-evil-state 'normal)))
      (let ((old-evil-move-cursor-back evil-move-cursor-back))
        (setq evil-move-cursor-back nil)
        (evil-change-state overlay-evil-state)
        (setq evil-move-cursor-back old-evil-move-cursor-back)))
     ((and (eq overlay-evil-state 'visual)
           (or (overlay-get o 'evil-visual-contracted)
               (eq evil-state 'normal)))
      (let ((p (point))
            (m (mark)))
        (evil-change-state overlay-evil-state)
        (evil-visual-refresh m p)))
     ((and (eq evil-state 'normal)
           (eq overlay-evil-state 'insert))
      (evil-change-state overlay-evil-state)
      ;; this is set when transitioning to insert state
      ;; most likely do not want this set
      ;; nothing I have has this set in ANY state
      (setq evil-maybe-remove-spaces nil))
     ((and (eq evil-state 'insert)
           (eq overlay-evil-state 'visual)
           (eq evil-visual-selection 'line))
      (evil-change-state overlay-evil-state)
      ;; the selection is `line' but the changing of states sets it to `char'
      ;; setting it back and refreshing
      (evil-visual-refresh (mark) (point) 'line))
     (t
      (evil-change-state overlay-evil-state)))))

(defun mc/evil-restore-state-from-overlay (o)
  "Restore state for fake cursor `O'. If no evil state change is
required, just restore the state in `mc/cursor-specific-vars' and
maybe refresh evil visual variables, otherwise perform state
transition and restore variables."
  (cond
   ((mc/overlay-change-evil-state-p o)
    (mc/evil-fake-cursor-state-transition o))
   (t
    (mc/restore-overlay-vars o mc/cursor-specific-vars)))
  (mc/evil-maybe-visual-refresh))

(defun mc/evil-read-key-advice (orig-fun &optional prompt)
  "Advice around `evil-read-key'. Cache the results when the main
cursor calls it, reuse that stored value when the fake cursor
calls the function."
  (if mc--executing-command-for-fake-cursor
      mc--evil-key-read-results
    (let ((res (funcall orig-fun prompt)))
      (setq mc--evil-key-read-results res)
      res)))

(defun mc/this-command-keys-advice (orig-fun)
  "Advice around `this-command-keys'. Cache the result of the
function when executing for the main cursor, no overwriting the
stored value if the function returns an empty string. Use stored
variable if called from a fake cursor."
  (if mc--executing-command-for-fake-cursor
      mc--this-command-keys-result
    (let ((res (funcall orig-fun)))
      (when (not (eq "" res))
        (setq mc--this-command-keys-result res))
      res)))

(defun mc/evil-read-motion-advice (orig-fun &optional motion count type modifier)
  "Advice around `mc/evil-read-motion'. Cache the result of the
function if executing for the main cursor. Read the cached value
if executing for a fake cursor. If executing for a fake cursor
and we are going to execute a kbd macro for the fake cursor, call
the function and don't cache the results."
  (if mc--executing-command-for-fake-cursor
      (if mc--this-kbd-macro-to-execute
          (apply orig-fun motion count type modifier)
        mc--evil-motion-read-results)
    (let ((res (apply orig-fun motion count type modifier)))
      (setq mc--evil-motion-read-results res)
      res)))

(defun mc/call-interactively-advice (orig-fun f &optional record-flag keys)
  "Advice around `call-interactively'. Always call the function
and return it's results. If the function `f' is an evil motion in
the variable `mc--evil-cmds-to-record-macro` and we are using
`evil' and the main cursor is executing, cache the kbd macro from
the current command keys vector and the cached value of
`mc--this-command-keys-result'. This is used when the function
`f' being called has itself, or something from its call chain, an
interactive call to a function with property `interactive' of
value `c'. No advice can wrap around the `read-char` call when
invoked in this way, so a macro can be recorded and executed to
properly get that interactive character read for the fake
cursor."
  (let ((res (funcall orig-fun f record-flag keys)))
    (when (and (memq f mc--evil-cmds-to-record-macro)
               (mc/evil-p)
               (not mc--executing-command-for-fake-cursor))
      (let ((kbd-macro (vconcat mc--this-command-keys-result
                                (this-command-keys-vector))))
        (setq mc--this-kbd-macro-to-execute kbd-macro)))
    res))

(defun mc/evil-repeat-pre-hook-advice (orig-fun)
  "Advice around `evil-repeat-pre-hook'. Only call the function
if executing for the main cursor."
  (unless mc--executing-command-for-fake-cursor
    (funcall orig-fun)))

(defun mc/evil-escape-pre-hook-advice (orig-fun)
  "Advice around `evil-escape-pre-command-hook'. Only call the
function if executing for the main cursor, setting
`this-original-command' if `this-command' is
`self-insert-command' and after this hooks execution, it is
something different."
  (unless mc--executing-command-for-fake-cursor
    (let ((cmd this-command))
      (funcall orig-fun)
      ;; `evil-escape' sets `this-command' not `this-original-command'
      ;; if the original value of `this-command' isn't the same as what it is set to,
      ;; we can safely reset `this-original-command'
      ;; to the new value of `this-command' that `evil-escape' has set.
      (if (not (eq cmd this-command))
          (setq this-original-command this-command)))))

(advice-add 'evil-read-key :around #'mc/evil-read-key-advice)
(advice-add 'this-command-keys :around #'mc/this-command-keys-advice)
(advice-add 'call-interactively :around #'mc/call-interactively-advice)
(advice-add 'evil-read-motion :around #'mc/evil-read-motion-advice)
(advice-add 'evil-repeat-pre-hook :around #'mc/evil-repeat-pre-hook-advice)
(when (and (featurep 'evil-escape)
           evil-escape-mode)
  (advice-add 'evil-escape-pre-command-hook :around #'mc/evil-escape-pre-hook-advice))

(provide 'mc-evil)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; mc-evil.el ends here
