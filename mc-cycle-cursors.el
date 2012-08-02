;;; mc-cycle-cursors.el

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

;; This scrolls the buffer to center each cursor in turn.
;; Scroll down with C-v, scroll up with M-v
;; This is nice when you have cursors that's outside of your view.

;;; Code:

(require 'multiple-cursors-core)

(eval-when-compile (require 'cl))

(defun mc/next-cursor-after-point ()
  (let ((pos (point))
        (next-pos (point-max))
        next)
    (mc/for-each-fake-cursor
     (let ((cursor-pos (overlay-get cursor 'point)))
       (when (and (< pos cursor-pos)
                  (< cursor-pos next-pos))
         (setq next-pos cursor-pos)
         (setq next cursor))))
    next))

(defun mc/prev-cursor-before-point ()
  (let ((pos (point))
        (prev-pos (point-min))
        prev)
    (mc/for-each-fake-cursor
     (let ((cursor-pos (overlay-get cursor 'point)))
       (when (and (> pos cursor-pos)
                  (> cursor-pos prev-pos))
         (setq prev-pos cursor-pos)
         (setq prev cursor))))
    prev))

(defun mc/cycle-forward ()
  (interactive)
  (let ((next-cursor (mc/next-cursor-after-point)))
    (unless next-cursor
      (error "We're already at the last cursor"))
    (mc/create-fake-cursor-at-point)
    (mc/pop-state-from-overlay next-cursor)
    (recenter)))

(defun mc/cycle-backward ()
  (interactive)
  (let ((prev-cursor (mc/prev-cursor-before-point)))
    (unless prev-cursor
      (error "We're already at the first cursor"))
    (mc/create-fake-cursor-at-point)
    (mc/pop-state-from-overlay prev-cursor)
    (recenter)))

(define-key mc/keymap (kbd "C-v") 'mc/cycle-forward)
(define-key mc/keymap (kbd "M-v") 'mc/cycle-backward)

(provide 'mc-cycle-cursors)

;;; mc-cycle-cursors.el ends here
