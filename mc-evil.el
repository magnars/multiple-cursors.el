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
  (and (featurep 'evil) evil-mode))

(defun mc/overlay-change-evil-state-p (o)
  (not (eq evil-state (overlay-get o 'evil-state))))

(defun mc/restore-overlay-vars (o vars)
  (dolist (var vars)
    (when (boundp var) (set var (overlay-get o var)))))

(defun mc/evil-restore-state-from-overlay (o)
  (cond
   ((mc/overlay-change-evil-state-p o)
    (let ((overlay-evil-state (overlay-get o 'evil-state))
          (cursor-specific-vars (cl-remove-if #'(lambda (var) (eq var 'evil-state)) mc/cursor-specific-vars)))
      (mc/restore-overlay-vars o cursor-specific-vars)
      (cond
       ((and (eq evil-state 'insert)
             (eq overlay-evil-state 'normal))
        (let ((old-evil-move-cursor-back evil-move-cursor-back))
          (setq evil-move-cursor-back nil)
          (evil-change-state overlay-evil-state)
          (setq evil-move-cursor-back old-evil-move-cursor-back)))
       (t
        (evil-change-state overlay-evil-state)))))
   (t (mc/restore-overlay-vars o mc/cursor-specific-vars))))


(provide 'mc-evil)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; mc-evil.el ends here
