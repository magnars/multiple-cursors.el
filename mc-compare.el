;;; mc-compare.el --- Compare texts in multiple-cursors mode.

;; Copyright (C) 2013 Akinori MUSHA

;; Author: Akinori MUSHA <knu@idaemons.org>
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

;; This library contains functions to compare texts in
;; multiple-cursors mode.

;;; Installation:

;;   (define-key mc/keymap (kbd "C-. =") 'mc/compare-chars)

;;; Code:

(eval-when-compile (require 'cl))
(require 'multiple-cursors-core)

;;;###autoload
(defun mc/compare-chars (&optional arg)
  "Compare the character at point with that at each fake cursor, and move forward as far as they all match.
With an optional argument, move backwards by calling `mc/compare-chars-backward'.
This command pushes the mark before moving cursors."
  (interactive "P")
  (if arg (mc/compare-chars-backward)
    (mc/compare-chars-forward)))

;;;###autoload
(defun mc/compare-chars-forward ()
  "Compare the character at point with that at each fake cursor, and move forward as far as they all match.
This command pushes the mark before moving cursors."
  (interactive)
  (let (current-prefix-arg)
    (mc/execute-command-for-all-cursors 'push-mark-command)
    (while (loop for cursor in (mc/all-fake-cursors)
                 with c = (following-char)
                 always (char-equal (char-after (overlay-start cursor)) c))
      (mc/execute-command-for-all-cursors 'forward-char))))

;;;###autoload
(defun mc/compare-chars-backward ()
  "Backwards version of `mc/compare-chars-forward'."
  (interactive)
  (let (current-prefix-arg)
    (mc/execute-command-for-all-cursors 'push-mark-command)
    (while (loop for cursor in (mc/all-fake-cursors)
                 with c = (preceding-char)
                 always (char-equal (char-before (overlay-start cursor)) c))
      (mc/execute-command-for-all-cursors 'backward-char))))

;;;###autoload
(eval-after-load 'multiple-cursors-core
  '(progn
     (add-to-list 'mc--default-cmds-to-run-once 'mc/compare-chars)
     (add-to-list 'mc--default-cmds-to-run-once 'mc/compare-chars-forward)
     (add-to-list 'mc--default-cmds-to-run-once 'mc/compare-chars-backward)
     ))

(provide 'mc-compare)

;;; mc-compare.el ends here
