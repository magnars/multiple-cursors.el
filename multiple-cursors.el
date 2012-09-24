;;; multiple-cursors.el --- An experiment in multiple cursors for emacs.

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

;; Multiple cursors for Emacs. This is some pretty crazy functionality, so yes,
;; there are kinks. Don't be afraid tho, I've been using it since 2011 with
;; great success and much merriment.
;;
;; ## Basic usage
;;
;; Start out with:
;;
;;     (require 'multiple-cursors)
;;
;; When you have an active region that spans multiple lines, the following will
;; add a cursor to each line:
;;
;;     (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;
;; When you want to add multiple cursors not based on continuous lines, but based on
;; keywords in the buffer, use:
;;
;;     (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;;     (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;     (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
;;
;; First mark the word, then add more cursors.
;;
;; To get out of multiple-cursors-mode, press `<return>` or `C-g`. The latter will
;; first disable multiple regions before disabling multiple cursors. If you want to
;; insert a newline in multiple-cursors-mode, use `C-j`.
;;
;;
;; ## More commands to play around with
;;
;; I've set up my key-bindings like so:
;;
;;     ;; From active region to multiple cursors:
;;     (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;;     (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
;;     (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)
;;
;; When you have an active region that spans multiple lines, the preceeding three
;; commands will add one cursor to each line.
;;
;;     ;; Rectangular region mode
;;     (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)
;;
;; Think of this one as `set-mark` except you're marking a rectangular region. It is
;; an exceedingly quick way of adding multiple cursors to multiple lines.
;;
;;     ;; Mark more like this
;;     (global-set-key (kbd "M-æ") 'mc/mark-all-like-this)
;;     (global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
;;     (global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
;;     (global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
;;     (global-set-key (kbd "M-å") 'mc/mark-all-in-region)
;;
;; Okay, yes, I have a crazy norwegian keyboard. Regardless, these will look at
;; whatever you've got selected at the moment, and mark more places like that in
;; the buffer.
;;
;; BTW, I highly recommend adding `mc/mark-next-like-this` to a key binding that's
;; right next to the key for `er/expand-region`.
;;
;;
;; ## Unknown commands
;;
;; Multiple-cursors uses two lists of commands to know what to do: the run-once list
;; and the run-for-all list. It comes with a set of defaults, but it would be beyond silly
;; to try and include all the known Emacs commands.
;;
;; So that's why multiple-cursors occasionally asks what to do about a command. It will
;; then remember your choice by saving it in `~/.emacs.d/.mc-lists.el`. You can change
;; the location with:
;;
;;     (setq mc/list-file "/my/preferred/file")
;;
;;
;; ## Known limitations
;;
;; * isearch-forward and isearch-backward aren't supported with multiple cursors.
;;   You should feel free to add a simplified version that can work with it.
;; * Commands run with `M-x` won't be repeated for all cursors.
;; * All key bindings that refer to lambdas are always run for all cursors. If you
;;   need to limit it, you will have to give it a name.
;; * Redo might screw with your cursors. Undo works very well.
;;
;;
;; ## Contribute
;;
;; Yes, please do. There's a suite of tests, so remember to add tests for your
;; specific feature, or I might break it later.
;;
;; You'll find the repo at:
;;
;;     https://github.com/magnars/multiple-cursors.el
;;
;; To fetch the test dependencies:
;;
;;     $ cd /path/to/multiple-cursors
;;     $ git submodule update --init
;;
;; Run the tests with:
;;
;;     $ ./util/ecukes/ecukes --graphical
;;
;;; Code:

(require 'mc-edit-lines)
(require 'mc-cycle-cursors)
(require 'mc-mark-more)
(require 'rectangular-region-mode)

(provide 'multiple-cursors)

;;; multiple-cursors.el ends here
