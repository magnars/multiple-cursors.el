;;; mc-insert-numbers.el

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

;; This file contains functions for adding increasing integers to
;; each cursor.

;; Please see multiple-cursors.el for more commentary.

;;; Code:

(require 'multiple-cursors-core)

;;;###autoload
(defun mc/insert-numbers (arg)
  (interactive "P")
  (setq mc--insert-numbers-number (or arg 0))
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'mc--insert-number-and-increase cursor)))

(defvar mc--insert-numbers-number 0)

(defun mc--insert-number-and-increase ()
  (interactive)
  (insert (number-to-string mc--insert-numbers-number))
  (setq mc--insert-numbers-number (1+ mc--insert-numbers-number)))

(provide 'mc-insert-numbers)
;;; mc-insert-numbers.el ends here
