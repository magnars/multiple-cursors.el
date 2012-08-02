;;; mc-mark-more.el

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

;; This file contains functions to mark more parts of the buffer.
;; See ./features/mark-more.feature for examples.

;; Please see multiple-cursors.el for more commentary.

;;; Code:

(require 'multiple-cursors-core)

(defun mc/cursor-end (cursor)
  (if (overlay-get cursor 'mark-active)
      (max (overlay-get cursor 'point)
           (overlay-get cursor 'mark))
    (overlay-get cursor 'point)))

(defun mc/cursor-beg (cursor)
  (if (overlay-get cursor 'mark-active)
      (min (overlay-get cursor 'point)
           (overlay-get cursor 'mark))
    (overlay-get cursor 'point)))

(defun mc/furthest-region-end ()
  (let ((end (max (mark) (point))))
    (mc/for-each-fake-cursor
     (setq end (max end (mc/cursor-end cursor))))
    end))

(defun mc/first-region-start ()
  (let ((beg (min (mark) (point))))
    (mc/for-each-fake-cursor
     (setq beg (min beg (mc/cursor-beg cursor))))
    beg))

(defun mc/furthest-cursor-before-point ()
  (let ((beg (min (mark) (point)))
        furthest)
    (mc/for-each-fake-cursor
     (when (< (mc/cursor-beg cursor) beg)
       (setq beg (mc/cursor-beg cursor))
       (setq furthest cursor)))
    furthest))

(defun mc/furthest-cursor-after-point ()
  (let ((end (max (mark) (point)))
        furthest)
    (mc/for-each-fake-cursor
     (when (> (mc/cursor-end cursor) end)
       (setq end (mc/cursor-end cursor))
       (setq furthest cursor)))
    furthest))

(defun mc/region-strings ()
  (let ((strings (list (buffer-substring-no-properties (point) (mark)))))
    (mc/for-each-fake-cursor
     (add-to-list 'strings (buffer-substring-no-properties
                            (mc/cursor-beg cursor)
                            (mc/cursor-end cursor))))
    strings))

;;;###autoload
(defun mc/mark-next-like-this (arg)
  "Find and mark the next part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (unless (region-active-p)
    (error "Mark a region to match first."))
  (when (< arg 0)
    (mc/remove-fake-cursor (mc/furthest-cursor-after-point)))
  (when (>= arg 0)
    (let ((case-fold-search nil)
          (point-first (< (point) (mark)))
          (re (regexp-opt (mc/region-strings)))
          (furthest-cursor (mc/furthest-cursor-after-point)))
      (mc/save-excursion
       (goto-char (mc/furthest-region-end))
       (when (= arg 0)
         (mc/remove-fake-cursor furthest-cursor))
       (if (search-forward-regexp re nil t)
           (progn
             (push-mark (match-beginning 0))
             (when point-first (exchange-point-and-mark))
             (mc/create-fake-cursor-at-point))
         (error "no more found forward")))))
  (if (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)
    (multiple-cursors-mode 0)))

;;;###autoload
(defun mc/mark-previous-like-this (arg)
  "Find and mark the previous part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (unless (region-active-p)
    (error "Mark a region to match first."))
  (when (< arg 0)
    (mc/remove-fake-cursor (mc/furthest-cursor-before-point)))
  (when (>= arg 0)
    (let ((case-fold-search nil)
          (point-first (< (point) (mark)))
          (re (regexp-opt (mc/region-strings)))
          (furthest-cursor (mc/furthest-cursor-before-point)))
      (mc/save-excursion
       (goto-char (mc/first-region-start))
       (when (= arg 0)
         (mc/remove-fake-cursor furthest-cursor))
       (if (search-backward-regexp re nil t)
           (progn
             (push-mark (match-end 0))
             (unless point-first (exchange-point-and-mark))
             (mc/create-fake-cursor-at-point))
         (error "no more found backward")))))
  (if (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)
    (multiple-cursors-mode 0)))

;;;###autoload
(defun mc/mark-all-like-this ()
  "Find and mark all the parts of the buffer matching the currently active region"
  (interactive)
  (unless (region-active-p)
    (error "Mark a region to match first."))
  (mc/remove-fake-cursors)
  (let ((master (point))
        (case-fold-search nil)
        (point-first (< (point) (mark)))
        (re (regexp-opt (mc/region-strings))))
    (mc/save-excursion
     (goto-char 0)
     (while (search-forward-regexp re nil t)
       (push-mark (match-beginning 0))
       (when point-first (exchange-point-and-mark))
       (unless (= master (point))
         (mc/create-fake-cursor-at-point))
       (when point-first (exchange-point-and-mark)))))
  (if (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)
    (multiple-cursors-mode 0)))

;;;###autoload
(defun mc/mark-all-in-region (beg end)
  "Find and mark all the parts in the region matching the given search"
  (interactive "r")
  (let ((search (read-from-minibuffer "Mark all in region: "))
        (case-fold-search nil))
    (mc/remove-fake-cursors)
    (goto-char beg)
    (while (search-forward search end t)
      (push-mark (match-beginning 0))
      (mc/create-fake-cursor-at-point))
    (let ((first (mc/furthest-cursor-before-point)))
      (if (not first)
          (error "Search failed for %S" search)
        (mc/pop-state-from-overlay first))))
  (if (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)
    (multiple-cursors-mode 0)))

;;;###autoload
(defun mc/mark-more-like-this-extended ()
  "Like mark-more-like-this, but then lets you adjust with arrows key.
The actual adjustment made depends on the final component of the
key-binding used to invoke the command, with all modifiers removed:

   <up>    Mark previous like this
   <down>  Mark next like this
   <left>  If last was previous, skip it
           If last was next, remove it
   <right> If last was next, skip it
           If last was previous, remove it

Then, continue to read input events and further add or move marks
as long as the input event read (with all modifiers removed)
is one of the above."
  (interactive)
  (let ((first t)
        (ev last-command-event)
        (cmd 'mc/mark-next-like-this)
        (arg 1)
        last echo-keystrokes)
    (while cmd
      (let ((base (event-basic-type ev)))
        (cond ((eq base 'left)
               (if (eq last 'mc/mark-previous-like-this)
                   (setq cmd last arg 0)
                 (setq cmd 'mc/mark-next-like-this arg -1)))
              ((eq base 'up)
               (setq cmd 'mc/mark-previous-like-this arg 1))
              ((eq base 'right)
               (if (eq last 'mc/mark-next-like-this)
                   (setq cmd last arg 0)
                 (setq cmd 'mc/mark-previous-like-this arg -1)))
              ((eq base 'down)
               (setq cmd 'mc/mark-next-like-this arg 1))
              (first
               (setq cmd 'mc/mark-next-like-this arg 1))
              (t
               (setq cmd nil))))
      (when cmd
        (ignore-errors
          (funcall cmd arg))
        (setq first nil last cmd)
        (setq ev (read-event "Use arrow keys for more marks: "))))
    (push ev unread-command-events)))

(provide 'mc-mark-more)

;;; mc-mark-more.el ends here
