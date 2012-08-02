;;; mc-mark-multiple-integration.el

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

;; When using mark-multiple, you can now use C-g to switch to multiple-cursors

;;; Code:

(require 'multiple-cursors-core)
(require 'mark-multiple)

(defun mc/switch-from-mark-multiple-to-cursors ()
  "Removes mark-multiple and switches to multiple cursors instead"
  (interactive)
  (let ((offset (- (point) (overlay-start mm/master))))
    (deactivate-mark)
    (save-excursion
      (dolist (mirror mm/mirrors)
        (goto-char (+ offset (overlay-start mirror)))
        (mc/create-fake-cursor-at-point)))
    (mm/clear-all)
    (multiple-cursors-mode)))

(define-key mm/keymap (kbd "C-g") 'mc/switch-from-mark-multiple-to-cursors)

(provide 'mc-mark-multiple-integration)

;;; mc-mark-multiple-integration.el ends here
