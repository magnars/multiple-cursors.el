;;; mark-more-like-this.el --- Mark additional regions in buffer matching current region.
;;
;; Copyright (C) 2011 Magnar Sveen
;;
;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking replace
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; These commands will look for strings in the buffer that matches your currently
;; active region and make them mirrors. The mirrors are updated inline as you type.
;;
;;     (require 'mark-more-like-this)
;;     (global-set-key (kbd "C-<") 'mark-previous-like-this)
;;     (global-set-key (kbd "C->") 'mark-next-like-this)
;;     (global-set-key (kbd "C-M-m") 'mark-more-like-this)
;;
;; You should feel free to make your own keybindings.
;;
;; 'mark-more-like-this marks the ARG next matches (previous if negative)
;;
;; 'mark-next-like-this marks the next occurance.
;;     - with a negative ARG, removes the last occurance.
;;     - with a zero ARG, skips the last occurance and marks the next.
;;
;; 'mark-previous-like-this works like -next- but in the other direction.
;;
;; This extension is dependent on the mark-multiple library.
;;     https://github.com/magnars/mark-multiple.el

;;; Code:

(require 'mark-multiple)

(defun mark-next-like-this (arg)
  "Find and mark the next part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark next."
  (interactive "p")
  (unless (or (region-active-p)
              mm/master)
    (error "Mark a region to match first."))
  (if (< arg 0)
      (mm/remove-mirror (mm/furthest-mirror-after-master)))
  (if (>= arg 0)
      (progn
        (when (null mm/master)
          (mm/create-master (region-beginning) (region-end)))

        (save-excursion
          (goto-char (mm/last-overlay-end))
          (if (= arg 0)
              (mm/remove-mirror (mm/furthest-mirror-after-master)))
          (let ((case-fold-search nil)
                (master-str (mm/master-substring)))
            (if (search-forward master-str nil t)
                (mm/add-mirror (- (point) (length master-str)) (point))
              (error "no more found \"%s\" forward"
                     (substring-no-properties master-str))))))))

(defun mark-previous-like-this (arg)
  "Find and mark the previous part of the buffer matching the currently active region
With negative ARG, delete the last one instead.
With zero ARG, skip the last one and mark previous."
  (interactive "p")
  (unless (or (region-active-p)
              mm/master)
    (error "Mark a region to match first."))
  (if (< arg 0)
      (mm/remove-mirror (mm/furthest-mirror-before-master)))
  (if (>= arg 0)
      (progn
        (when (null mm/master)
          (mm/create-master (region-beginning) (region-end)))

        (save-excursion
          (goto-char (mm/first-overlay-start))
          (if (= arg 0)
              (mm/remove-mirror (mm/furthest-mirror-before-master)))
          (let ((case-fold-search nil)
                (master-str (mm/master-substring)))
            (if (search-backward master-str nil t)
                (mm/add-mirror (point) (+ (point) (length master-str)))
              (error "no more found \"%s\" backward"
                     (substring-no-properties master-str))))))))

(defun mark-all-like-this ()
  "Find and mark all the parts of the buffer matching the currently active region"
  (interactive)
  (unless (or (region-active-p) mm/master) (error "Mark a region to match first."))
  (if (not mm/master)
      (mm/create-master (region-beginning) (region-end)))
  (dolist (mirror mm/mirrors)
    (delete-overlay mirror))
  (setq mm/mirrors ())
  (save-excursion
    (goto-char 0)
    (let ((case-fold-search nil)
          (master-str (mm/master-substring)))
      (while (search-forward master-str nil t)
        (let ((start (- (point) (length master-str)))
              (end (point)))
          (if (/= (overlay-start mm/master) start)
              (mm/add-mirror start end)))))))

(defun mark-all-like-this-in-region (reg-start reg-end)
  "Find and mark all the parts in the region matching the given search"
  (interactive "r")
  (let ((search (read-from-minibuffer "Mark all in region: "))
        (case-fold-search nil))
    (deactivate-mark)
    (save-excursion
      (goto-char reg-start)
      (while (search-forward search reg-end t)
        (mm/create-master-or-mirror (- (point) (length search)) (point))))
    (unless mm/master
      (error "Search failed for %S" search))
    (goto-char (mm/master-start))))

(defun mark-more-like-this (arg)
  "Marks next part of buffer that matches the currently active region ARG times.
Given a negative ARG it searches backwards instead."
  (interactive "p")
  (unless (or (region-active-p)
              mm/master)
    (error "Mark a region to match first."))
  (if (> arg 0)
      (dotimes (i arg) (mark-next-like-this 1))
    (dotimes (i (- arg)) (mark-previous-like-this 1))))

(defun mark-more-like-this-extended ()
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
        (cmd 'mark-next-like-this)
        (arg 1)
        last echo-keystrokes)
    (while cmd
      (let ((base (event-basic-type ev)))
        (cond ((eq base 'left)
               (if (eq last 'mark-previous-like-this)
                   (setq cmd last arg 0)
                 (setq cmd 'mark-next-like-this arg -1)))
              ((eq base 'up)
               (setq cmd 'mark-previous-like-this arg 1))
              ((eq base 'right)
               (if (eq last 'mark-next-like-this)
                   (setq cmd last arg 0)
                 (setq cmd 'mark-previous-like-this arg -1)))
              ((eq base 'down)
               (setq cmd 'mark-next-like-this arg 1))
              (first
               (setq cmd 'mark-next-like-this arg 1))
              (t
               (setq cmd nil))))
      (when cmd
        (ignore-errors
          (funcall cmd arg))
        (setq first nil last cmd)
        (setq ev (read-event "Use arrow keys for more marks: "))))
    (push ev unread-command-events)))

(provide 'mark-more-like-this)

;;; mark-more-like-this.el ends here
