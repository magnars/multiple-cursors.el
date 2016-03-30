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

(defun mc/evil-p ()
  (and (featurep 'evil) evil-mode))

(defun mc/evil-adjust-mark (direction)
  "Adjust mark when evil-mode is enabled.

Evil has a different notion of cursor position than vanilla Emacs.
This function adjusts the mark for this off-by-one error."
  (when (mc/evil-p)
    (ecase direction
      (forwards  (goto-char (1- (point))))
      (backwards (push-mark (1- (mark)))))))

(defun mc/evil-visual-state ()
  (when (mc/evil-p)
    (setq evil-state 'visual)))

(defun mc/evil-visual-refresh ()
  "Refreshes the Evil visual range based on the current mark and point."
  (evil-visual-refresh (mark) (point)))

(defun mc/evil-read-motion-advice (orig-fun &optional motion count type modifier)
  (if mc--executing-command-for-fake-cursor
      mc--evil-motion-read-results
    (let ((res (apply orig-fun motion count type modifier)))
      (setq mc--evil-motion-read-results res)
      res)))

(advice-add 'evil-read-motion :around #'mc/evil-read-motion-advice)

(provide 'mc-evil)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; mc-evil.el ends here
