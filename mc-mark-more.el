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

(defvar mc/enclose-search-term nil
  "How should mc/mark-more-* search for more matches?

Match everything: nil
Match only whole words: 'words
Match only whole symbols: 'symbols

Use like case-fold-search, don't recommend setting it globally.")

(defun mc/mark-more-like-this (skip-last direction)
  (let ((case-fold-search nil)
        (re (regexp-opt (mc/region-strings) mc/enclose-search-term))
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
       (error "no more matches found.")))))

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
(defun mc/mark-next-word-like-this (arg)
  (interactive "p")
  (let ((mc/enclose-search-term 'words))
    (mc/mark-next-like-this arg)))

;;;###autoload
(defun mc/mark-next-symbol-like-this (arg)
  (interactive "p")
  (let ((mc/enclose-search-term 'symbols))
    (mc/mark-next-like-this arg)))

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

;;;###autoload
(defun mc/mark-previous-word-like-this (arg)
  (interactive "p")
  (let ((mc/enclose-search-term 'words))
    (mc/mark-previous-like-this arg)))

;;;###autoload
(defun mc/mark-previous-symbol-like-this (arg)
  (interactive "p")
  (let ((mc/enclose-search-term 'symbols))
    (mc/mark-previous-like-this arg)))

(defun mc/mark-lines (num-lines direction)
  (dotimes (i num-lines)
    (mc/create-fake-cursor-at-point)
    (ecase direction
      (forwards (loop do (next-line 1 nil)
                      while (mc/all-fake-cursors (point) (1+ (point)))))
      (backwards (loop do (previous-line 1 nil)
                       while (mc/all-fake-cursors (point) (1+ (point))))))))

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
(defun mc/mark-all-like-this ()
  "Find and mark all the parts of the buffer matching the currently active region"
  (interactive)
  (unless (region-active-p)
    (error "Mark a region to match first."))
  (mc/remove-fake-cursors)
  (let ((master (point))
        (case-fold-search nil)
        (point-first (< (point) (mark)))
        (re (regexp-opt (mc/region-strings) mc/enclose-search-term)))
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
(defun mc/mark-all-words-like-this ()
  (interactive)
  (let ((mc/enclose-search-term 'words))
    (mc/mark-all-like-this)))

;;;###autoload
(defun mc/mark-all-symbols-like-this ()
  (interactive)
  (let ((mc/enclose-search-term 'symbols))
    (mc/mark-all-like-this)))

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

(defun mc/mark-all-like-this-dwim (arg)
  "Uses some sane defaults to guess what the user want to do:

- If inside a defun, find and mark all the parts of current defun matchign
the currently active region. If no region is active, activate the word
under cursor.
- If in SGML/HTML mode and inside a tag, select the tag and its pair

With prefix, it behaves the same as original `mc/mark-all-like-this'"
  (interactive "P")
  (if arg
      (mc/mark-all-like-this)
    (let ((mode (with-current-buffer (current-buffer) major-mode)))
      (cond ((and (member mode '(sgml-mode html-mode))
                  (mc/mark-tags)) t)
            ((bounds-of-thing-at-point 'defun)
             (mc/select-under-cursor)
             (save-restriction
               (widen)
               (narrow-to-defun)
               (mc/mark-all-like-this)))
            (t (mc/select-under-cursor) (mc/mark-all-like-this))))))

(defun mc/select-under-cursor ()
  "Select the word under cursor"
  (interactive)
  (when (not (use-region-p))
    (let ((b (bounds-of-thing-at-point 'word)))
      (goto-char (car b))
      (set-mark (cdr b)))))

(defun mc/mark-tags ()
  "Mark the tag we're in and its pair for renaming."
  (interactive)
  (let ((context (car (last (save-excursion (sgml-get-context))))))
    (when (and context
               (> (point) (aref context 2))
               (< (point) (aref context 3)))
      (let* ((tag-position (aref context 1))
             (tag-length (length (aref context 4)))
             (main-start (- (aref context 3) 1 tag-length))
             (mirror-start (save-excursion
                             (if (eq tag-position 'open)
                                 (sgml-skip-tag-forward 1)
                               (sgml-skip-tag-backward 1)
                               (forward-sexp))
                             (- (point) 1 tag-length))))
        (goto-char main-start)
        (set-mark (+ main-start tag-length))
        (mc/save-excursion (goto-char mirror-start)
                           (push-mark (+ mirror-start tag-length))
                           (mc/create-fake-cursor-at-point))
        (mc/maybe-multiple-cursors-mode)))))

(provide 'mc-mark-more)

;;; mc-mark-more.el ends here
