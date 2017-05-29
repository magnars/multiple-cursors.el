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
       ((and (eq overlay-evil-state 'visual)
             (overlay-get o 'evil-visual-contracted))
        (let ((p (point))
              (m (mark)))
          (evil-change-state overlay-evil-state)
          (evil-visual-refresh p m)))
       ((and (eq evil-state 'normal)
             (eq overlay-evil-state 'insert))
        (evil-change-state overlay-evil-state)
        ;; this is set when transitioning to insert state
        ;; most likely do not want this set
        ;; nothing I have has this set in ANY state
        (setq evil-maybe-remove-spaces nil))
       ((and (eq evil-state 'replace)
             (eq overlay-evil-state 'normal))
        (setq evil-move-cursor-back nil)
        (evil-change-state overlay-evil-state)
        (setq evil-move-cursor-back t))
       (t
        (evil-change-state overlay-evil-state)))))
   (t (mc/restore-overlay-vars o mc/cursor-specific-vars))))

(defun mc/evil-read-key-advice (orig-fun &optional prompt)
  (if mc--executing-command-for-fake-cursor
      mc--evil-key-read-results
    (let ((res (funcall orig-fun prompt)))
      (setq mc--evil-key-read-results res)
      res)))

(defun mc/this-command-keys-advice (orig-fun)
  (if mc--executing-command-for-fake-cursor
      mc--this-command-keys-result
    (let ((res (funcall orig-fun)))
      (setq mc--this-command-keys-result res)
      res)))

(defun mc/evil-read-motion-advice (orig-fun &optional motion count type modifier)
  (if mc--executing-command-for-fake-cursor
      mc--evil-motion-read-results
    (let ((res (apply orig-fun motion count type modifier)))
      (setq mc--evil-motion-read-results res)
      res)))

(defun mc/evil-repeat-pre-hook-advice (orig-fun)
  (unless mc--executing-command-for-fake-cursor
    (funcall orig-fun)))

(advice-add 'evil-read-key :around #'mc/evil-read-key-advice)
(advice-add 'this-command-keys :around #'mc/this-command-keys-advice)
(advice-add 'evil-read-motion :around #'mc/evil-read-motion-advice)
(advice-add 'evil-repeat-pre-hook :around #'mc/evil-repeat-pre-hook-advice)

(provide 'mc-evil)

;; Local Variables:
;; coding: utf-8
;; byte-compile-warnings: (not cl-functions)
;; End:

;;; mc-evil.el ends here
