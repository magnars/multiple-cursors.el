;;; mc-edit-lines.el

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

;; This file contains functions to add multiple cursors to consecutive lines
;; given an active region.

;; Please see multiple-cursors.el for more commentary.

;;; Code:

(require 'multiple-cursors-core)

;;;###autoload
(defun mc/edit-lines ()
  "Add one cursor to each line of the active region.
Starts from mark and moves in straight down or up towards the
line point is on."
  (interactive)
  (when (not (and mark-active (/= (point) (mark))))
    (error "Mark a set of lines first."))
  (mc/remove-fake-cursors)
  (let* ((col (current-column))
         (point-line (line-number-at-pos))
         (mark-line (progn (exchange-point-and-mark) (line-number-at-pos)))
         (direction (if (< point-line mark-line) :up :down)))
    (deactivate-mark)
    (when (and (eq direction :up) (bolp))
      (previous-logical-line 1 nil)
      (move-to-column col))
    (while (not (eq (line-number-at-pos) point-line))
      (mc/create-fake-cursor-at-point)
      (if (eq direction :up)
          (previous-logical-line 1 nil)
        (next-logical-line 1 nil))
      (move-to-column col))
    (multiple-cursors-mode)))

;;;###autoload
(defun mc/edit-ends-of-lines ()
  "Add one cursor to the end of each line in the active region."
  (interactive)
  (mc/edit-lines)
  (mc/execute-command-for-all-cursors 'end-of-line))

;;;###autoload
(defun mc/edit-beginnings-of-lines ()
  "Add one cursor to the beginning of each line in the active region."
  (interactive)
  (mc/edit-lines)
  (mc/execute-command-for-all-cursors 'beginning-of-line))

(provide 'mc-edit-lines)

;;; mc-edit-lines.el ends here
