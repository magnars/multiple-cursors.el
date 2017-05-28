;;; multiple-cursors-core.el --- An experiment in multiple cursors for emacs.

;; Copyright (C) 2012-2016 Magnar Sveen

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

(defcustom mc/list-file (locate-user-emacs-file ".mc-lists.el")
  "The position of the file that keeps track of your preferences
for running commands with multiple cursors."
  :type 'file
  :group 'multiple-cursors)

(defvar mc--evil-cursor-specific-vars '(evil-state
                                        evil-previous-state
                                        evil-previous-state-alist
                                        evil-next-state
                                        evil-normal-state-entry-hook
                                        evil-repeat-ring)
  "List of variables to keep track of on a per cursor basis when using evil")

(defvar mc--evil-cmds-to-run-for-all
  '(
    ;; state toggles
    evil-insert
    evil-normal-state
    evil-replace-state
    evil-exit-visual-state
    evil-visual-char ;; v in normal state
    evil-visual-line ;; V in normal state
    evil-append
    ;; evil-commands
    evil-delete-backward-char-and-join
    evil-force-normal-state
    evil-repeat
    ;; evil-operators
    evil-change
    evil-change-line
    evil-delete
    evil-delete-char
    evil-delete-line
    evil-replace
    ;; evil-motions
    evil-backward-char
    evil-backward-word-begin
    evil-find-char
    evil-find-char-to
    evil-forward-char
    evil-forward-word-begin
    evil-forward-word-end
    evil-next-line
    evil-repeat-find-char
    evil-repeat-find-char-reverse
    ;; evil functions
    evil-append-line
    evil-insert-line
    evil-open-above
    evil-open-below
    evil-replace-backspace
    ;; misc for tests
    electric-newline-and-maybe-indent)
  "List of functions to run for all cursors when using evil.")

(defvar mc--this-command-keys-result nil
  "Stores the last pressed keys that executed the main cursor's
  command. This function returns the wrong results for fake
  cursors.")

(defvar mc--evil-motion-read-results nil
  "Stored results from the last call to `evil-motion-read'. The
  results are stored so that a motion doesn't need to be read by
  each fake cursor.")

(defvar mc--evil-key-read-results nil
  "Stored results from the last call to `evil-key-read'. The
  results are stored so that a key doesn't need to be read by
  each fake cursor.")

(provide 'mc-vars)
;;; mc-vars.el ends here
