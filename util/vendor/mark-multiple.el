;;; mark-multiple.el --- A library that sorta lets you mark several regions at once

;; Copyright (C) 2011 Magnar Sveen

;; Author: Magnar Sveen <magnars@gmail.com>
;; Keywords: marking library

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

;; An emacs extension that sorta lets you mark several regions at once.
;;
;; More precisely, it allows for one master region, with several mirror
;; regions. The mirrors are updated inline while you type. This allows for some
;; awesome functionality. Or at least, some more visually pleasing insert and
;; replace operations.
;;
;; Video
;; -----
;; You can [watch an intro to mark-multiple at Emacs Rocks](http://emacsrocks.com/e08.html).
;;
;; Done
;; ----
;; * A general library for managing master and mirrors
;; * `mark-more-like-this` which selects next/previous substring in the buffer that
;;   matches the current region.
;; * `inline-string-rectangle` which works like `string-rectangle` but lets you
;;   write inline - making it less error prone.
;; * `rename-sgml-tag` which updates the matching tag while typing.
;; * `js2-rename-var` which renames the variable on point and all occurrences
;;   in its lexical scope.
;;
;; Installation
;; ------------
;;
;;     git submodule add https://github.com/magnars/mark-multiple.el.git site-lisp/mark-multiple
;;
;; Then add the modules you want to your init-file:
;;
;;     (require 'inline-string-rectangle)
;;     (global-set-key (kbd "C-x r t") 'inline-string-rectangle)
;;
;;     (require 'mark-more-like-this)
;;     (global-set-key (kbd "C-<") 'mark-previous-like-this)
;;     (global-set-key (kbd "C->") 'mark-next-like-this)
;;     (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
;;
;;     (require 'rename-sgml-tag)
;;     (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)
;;
;; Feel free to come up with your own keybindings.
;;
;; Ideas for more
;; --------------
;; * `js-rename-local-var` which renames the variable at point in the local file.
;;
;; Bugs and gotchas
;; ----------------
;; * Adding a master and mirrors does not remove the active region. This might feel
;;   strange, but turns out to be practical.
;;
;; * The current mark-multiple general library lets you do stupid shit, like adding
;;   overlapping mirrors. That's only a problem for people who want to write their
;;   own functions using `mm/create-master` and `mm/add-mirror`.
;;
;; * Seems like there is some conflict with undo-tree.el, which sometimes clobbers
;;   the undo history. I might be doing something particularly stupid. Looking into it.
;;
;; * Reverting the buffer with active marks makes them unremovable.
;;
;; A wild idea
;; -----------
;;
;; Is this a subset of a crazy multiple-point module? How would that even work?
;;
;; There is one use for it I can see, which is editing the end of lines. Set up one
;; cursor at the end of each line, then just edit normally. The command is repeated
;; for each position.
;;
;; Might be too far out there. I still want to do edit-end-of-lines tho.
;;

;;; Code:

(defvar mm/master nil
  "The master overlay has the point. Moving point out of master clears all.")

(defvar mm/mirrors nil
  "A list of overlays that mirrors master after each change.")

(make-variable-buffer-local 'mm/master)
(make-variable-buffer-local 'mm/mirrors)

(defvar mm/keymap (make-sparse-keymap))
(define-key mm/keymap (kbd "C-g") 'mm/deactivate-region-or-clear-all)
(define-key mm/keymap (kbd "C-m") 'mm/deactivate-region-and-clear-all)
(define-key mm/keymap (kbd "<return>") 'mm/deactivate-region-and-clear-all)

(defface mm/master-face
  '((((class color) (background light)) (:background "DarkSeaGreen1"))
    (t (:background "DimGrey")))
  "The face used to highlight master"
  :group 'mark-multiple)

(defface mm/mirror-face
  '((((class color) (background light)) (:background "DarkSeaGreen1"))
    (t (:background "DimGrey")))
  "The face used to highlight mirror"
  :group 'mark-multiple)

(defun mm/create-master (start end)
  "Start a new multiple mark selection by defining the master region from START to END.
Point must be within the region."
  (if (or (< (point) start)
          (> (point) end))
      (error "Point must be inside master region"))
  (mm/clear-all)
  (setq mm/master
        (make-overlay start end nil nil t))
  (overlay-put mm/master 'priority 100)
  (overlay-put mm/master 'face 'mm/master-face)
  (overlay-put mm/master 'keymap mm/keymap)
  (overlay-put mm/master 'modification-hooks '(mm/on-master-modification))
  (overlay-put mm/master 'insert-in-front-hooks '(mm/on-master-modification))
  (overlay-put mm/master 'insert-behind-hooks '(mm/on-master-modification))
  (setq mm/mirrors ())
  (add-hook 'post-command-hook 'mm/post-command-handler nil t))

(defun mm/add-mirror (start end)
  "Add a region START to END that will mirror the current master."
  (if (null mm/master)
      (error "No master defined to mirror. Start with mm/create-master."))
  (let ((mirror (make-overlay start end nil nil t)))
    (setq mm/mirrors (cons mirror mm/mirrors))
    (overlay-put mirror 'priority 100)
    (overlay-put mirror 'face 'mm/mirror-face)))

(defun mm/deactivate-region-or-clear-all ()
  "Deactivate mark if active, otherwise clear all."
  (interactive)
  (if (use-region-p)
      (deactivate-mark)
    (mm/clear-all)))

(defun mm/deactivate-region-and-clear-all ()
  "Deactivate mark and clear all."
  (interactive)
  (deactivate-mark)
  (mm/clear-all))

(defun mm/clear-all ()
  "Remove all marks"
  (interactive)
  (when (overlayp mm/master)
    (delete-overlay mm/master)
    (dolist (mirror mm/mirrors)
      (delete-overlay mirror))
    (setq mm/master nil)
    (setq mm/mirrors ())
    (remove-hook 'post-command-hook 'mm/post-command-handler)))

(defun mm/master-start ()
  (overlay-start mm/master))

(defun mm/master-end ()
  (overlay-end mm/master))

(defun mm/point-is-outside-of-master ()
  "Is point outside of master?"
  (or (null mm/master)
      (< (point) (mm/master-start))
      (> (point) (mm/master-end))))

(defun mm/active-region-is-outside-of-master ()
  "Is region active and mark outside master?"
  (and (region-active-p)
       (or (< (mark) (mm/master-start))
           (> (mark) (mm/master-end)))))

(defun mm/post-command-handler ()
  "Clear all marks if point or region is outside of master"
  (if (or (mm/point-is-outside-of-master)
          (mm/active-region-is-outside-of-master))
      (mm/clear-all)))

(defun mm/master-substring ()
  "Get the buffer substring that is in master"
  (buffer-substring (mm/master-start) (mm/master-end)))

(defun mm/on-master-modification (overlay after? beg end &optional length)
  "Update all mirrors after a change"
  (save-excursion
    (dolist (mirror mm/mirrors)
      (mm/replace-mirror-substring mirror (mm/master-substring)))))

(defun mm/replace-mirror-substring (mirror substring)
  "Replace the contents of MIRROR with SUBSTRING"
  (goto-char (overlay-start mirror))
  (delete-char (- (overlay-end mirror) (overlay-start mirror)))
  (insert substring))

;; Define some utility functions for users of mark-multiple:

(defun mm/create-master-or-mirror (start end)
  "Create master from START to END if there is none, otherwise add mirror."
  (if (null mm/master)
      (mm/create-master start end)
    (mm/add-mirror start end)))

(defun mm/remove-mirror (mirror)
  "Removes all traces of MIRROR"
  (setq mm/mirrors (remove mirror mm/mirrors))
  (delete-overlay mirror))

(defun mm/furthest-mirror-before-master ()
  "Find the mirror with the lowest start position before master"
  (if (null mm/mirrors)
      (error "No mirrors to be found, sir."))
  (let ((first nil)
        (start (mm/master-start)))
    (dolist (mirror mm/mirrors)
      (when (< (overlay-start mirror) start)
        (setq first mirror)
        (setq start (overlay-start mirror))))
    first))

(defun mm/furthest-mirror-after-master ()
  "Find the mirror with the highest end position after master"
  (if (null mm/mirrors)
      (error "No mirrors to be found, sir."))
  (let ((last nil)
        (end (mm/master-end)))
    (dolist (mirror mm/mirrors)
      (when (> (overlay-end mirror) end)
        (setq last mirror)
        (setq end (overlay-end mirror))))
    last))

(defun mm/first-overlay-start ()
  "Find first buffer position covered by master and mirrors"
  (let ((start (mm/master-start)))
    (dolist (mirror mm/mirrors)
      (if (< (overlay-start mirror) start)
          (setq start (overlay-start mirror))))
    start))

(defun mm/last-overlay-end ()
  "Find last buffer position covered by master and mirrors"
  (let ((end (mm/master-end)))
    (dolist (mirror mm/mirrors)
      (if (> (overlay-end mirror) end)
          (setq end (overlay-end mirror))))
    end))

(provide 'mark-multiple)

;;; mark-multiple.el ends here
