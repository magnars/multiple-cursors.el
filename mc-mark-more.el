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
(require 'thingatpt)

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

(defvar mc--restrict-mark-all-to-symbols nil)

;;;###autoload
(defun mc/mark-all-like-this-dwim (arg)
  "Tries to guess what you want to mark all of.
Can be pressed multiple times to increase selection.

With prefix, it behaves the same as original `mc/mark-all-like-this'"
  (interactive "P")
  (if arg
      (mc/mark-all-like-this)
    (if (and (mc--no-region-and-in-sgmlish-mode)
             (mc--on-tag-name-p))
        (mc/mark-sgml-tag-pair)
      (let ((before (mc/num-cursors)))
        (unless (eq last-command 'mc/mark-all-like-this-dwim)
          (setq mc--restrict-mark-all-to-symbols nil))
        (unless (use-region-p)
          (mc--mark-symbol-at-point)
          (setq mc--restrict-mark-all-to-symbols t))
        (if mc--restrict-mark-all-to-symbols
            (mc/mark-all-symbols-like-this-in-defun)
          (mc/mark-all-like-this-in-defun))
        (when (<= (mc/num-cursors) before)
          (if mc--restrict-mark-all-to-symbols
              (mc/mark-all-symbols-like-this)
            (mc/mark-all-like-this)))
        (when (<= (mc/num-cursors) before)
          (mc/mark-all-like-this))))))

(defun mc--no-region-and-in-sgmlish-mode ()
  (and (not (use-region-p))
       (derived-mode-p 'sgml-mode)))

(defun mc--in-defun ()
  (bounds-of-thing-at-point 'defun))

;;;###autoload
(defun mc/mark-all-like-this-in-defun ()
  "Mark all like this in defun."
  (interactive)
  (if (mc--in-defun)
      (save-restriction
        (widen)
        (narrow-to-defun)
        (mc/mark-all-like-this))
    (mc/mark-all-like-this)))

;;;###autoload
(defun mc/mark-all-words-like-this-in-defun ()
  "Mark all words like this in defun."
  (interactive)
  (if (mc--in-defun)
      (save-restriction
        (widen)
        (narrow-to-defun)
        (mc/mark-all-words-like-this))
    (mc/mark-all-words-like-this)))

;;;###autoload
(defun mc/mark-all-symbols-like-this-in-defun ()
  "Mark all symbols like this in defun."
  (interactive)
  (if (mc--in-defun)
      (save-restriction
        (widen)
        (narrow-to-defun)
        (mc/mark-all-symbols-like-this))
    (mc/mark-all-symbols-like-this)))

(defun mc--mark-symbol-at-point ()
  "Select the symbol under cursor"
  (interactive)
  (when (not (use-region-p))
    (let ((b (bounds-of-thing-at-point 'symbol)))
      (goto-char (car b))
      (set-mark (cdr b)))))

(defun mc--get-nice-sgml-context ()
  (car
   (last
    (progn
      (when (looking-at "<") (forward-char 1))
      (when (looking-back ">") (forward-char -1))
      (sgml-get-context)))))

(defun mc--on-tag-name-p ()
  (let* ((context (save-excursion (mc--get-nice-sgml-context)))
         (tag-name-len (length (aref context 4)))
         (beg (aref context 2))
         (end (+ beg tag-name-len (if (eq 'open (aref context 1)) 1 3))))
    (and context
         (>= (point) beg)
         (<= (point) end))))

;;;###autoload
(defun mc/mark-sgml-tag-pair ()
  "Mark the tag we're in and its pair for renaming."
  (interactive)
  (when (not (mc--inside-tag-p))
    (error "Place point inside tag to rename."))
  (let ((context (mc--get-nice-sgml-context)))
    (if (looking-at "</")
        (setq context (car (last (sgml-get-context)))))
    (goto-char (aref context 2))
    (let* ((tag-name (aref context 4))
           (num-chars (length tag-name))
           (master-start (1+ (point)))
           (mirror-end (save-excursion
                         (sgml-skip-tag-forward 1)
                         (1- (point)))))
      (goto-char (- mirror-end num-chars))
      (set-mark mirror-end)
      (mc/create-fake-cursor-at-point)
      (goto-char master-start)
      (set-mark (+ (point) num-chars))))
  (mc/maybe-multiple-cursors-mode))

(defun mc--inside-tag-p ()
  (save-excursion
    (not (null (sgml-get-context)))))

(provide 'mc-mark-more)

;;; mc-mark-more.el ends here
