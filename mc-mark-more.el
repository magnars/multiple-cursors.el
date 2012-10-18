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

(defun mc/maybe-multiple-cursors-mode ()
  "Enable multiple-cursors-mode if there is more than one currently active cursor."
  (if (> (mc/num-cursors) 1)
      (multiple-cursors-mode 1)
    (multiple-cursors-mode 0)))

(defun mc/mark-more-like-this (skip-last direction)
  (let ((case-fold-search nil)
        (re (regexp-opt (mc/region-strings)))
        (point-out-of-order (ecase direction
                              (forwards       (< (point) (mark)))
                              (backwards (not (< (point) (mark))))))
        (furthest-cursor (ecase direction
                           (forwards  (mc/furthest-cursor-after-point))
                           (backwards (mc/furthest-cursor-before-point))))
        (start-char (ecase direction
                      (forwards  (mc/furthest-region-end))
                      (backwards (mc/first-region-start))))
        (search-function (ecase direction
                           (forwards  'search-forward-regexp)
                           (backwards 'search-backward-regexp)))
        (match-point-getter (ecase direction
                              (forwards 'match-beginning)
                              (backwards 'match-end))))
    (mc/save-excursion
     (goto-char start-char)
     (when skip-last
       (mc/remove-fake-cursor furthest-cursor))
     (if (funcall search-function re nil t) 
         (progn
           (push-mark (funcall match-point-getter 0))
           (when point-out-of-order
             (exchange-point-and-mark))
           (mc/create-fake-cursor-at-point))
       (mc/error "no more matches found.")))))

;;;###autoload
(defun mc/mark-next-like-this (arg)
  "Find and mark the next part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (region-active-p)
      (if (< arg 0)
          (mc/remove-fake-cursor (mc/furthest-cursor-after-point))
        (mc/mark-more-like-this (= arg 0) 'forwards))
    (mc/mark-lines arg 'forwards))
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun mc/mark-previous-like-this (arg)
  "Find and mark the previous part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (if (region-active-p)
      (if (< arg 0)
          (mc/remove-fake-cursor (mc/furthest-cursor-before-point))
        (mc/mark-more-like-this (= arg 0) 'backwards))
    (mc/mark-lines arg 'backwards))
  (mc/maybe-multiple-cursors-mode))

(defun mc/have-previous-line ()
  (not (= (point-min) (line-beginning-position))))

(defun mc/have-next-line ()
  (if (= 0 (current-column))
      ;; special case of allowing use to mark down past the trailing new line
      (not (eobp))
    (save-excursion
      (forward-line 1)
      (not (eobp)))))

(defun* mc/mark-lines (num-lines direction)
  "Puts cursors on the next NUM-LINES of buffer. Calls mc/error
if there are no more lines in the buffer."
  (flet ((have-next-line () (ecase direction
                              (forwards (mc/have-next-line))
                              (backwards (mc/have-previous-line))))
         (next-line () (ecase direction
                         (forwards (forward-line 1))
                         (backwards (forward-line -1)))))
    (dotimes (i num-lines)
      (unless (have-next-line)
        (return-from mc/mark-lines (mc/error "No more lines available.")))
      ;; mark here
      (mc/ensure-fake-cursor-at-point)
      ;; jump over any already existing marks (this is so one can use
      ;; mark-next/mark-previosu in whatever order to 'grow' the
      ;; marked lines.
      (loop do (next-line)
            while (mc/all-fake-cursors (point) (1+ (point)))))))

;;;###autoload
(defun mc/mark-next-lines (arg)
  (interactive "p")
  (mc/mark-lines arg 'forwards)
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun mc/mark-previous-lines (arg)
  (interactive "p")
  (mc/mark-lines arg 'backwards)
  (mc/maybe-multiple-cursors-mode))

;;;###autoload
(defun mc/unmark-next-like-this (arg)
  "Deselect next part of the buffer matching the currently active region."
  (interactive)
  (mc/mark-next-like-this -1))

;;;###autoload
(defun mc/unmark-previous-like-this (arg)
  "Deselect prev part of the buffer matching the currently active region."
  (interactive)
  (mc/mark-previous-like-this -1))

;;;###autoload
(defun* mc/mark-all-like-this ()
  "Find and mark all the parts of the buffer matching the currently active region"
  (interactive)
  (unless (region-active-p)
    (return-from mc/mark-all-like-this
      (mc/error "Mark a region to match first.")))
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
(defun* mc/mark-all-in-region (beg end)
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
      (unless first
        (return-from mc/mark-all-in-region
          (mc/error "Search failed for %S" search)))
      (mc/pop-state-from-overlay first)))
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
