(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq multiple-cursors-root-path project-directory)
  (setq multiple-cursors-util-path (expand-file-name "util" project-directory)))

(add-to-list 'load-path multiple-cursors-root-path)
(add-to-list 'load-path multiple-cursors-util-path)
(add-to-list 'load-path (expand-file-name "espuds" multiple-cursors-util-path))
(add-to-list 'load-path (expand-file-name "vendor" multiple-cursors-util-path))

(require 'multiple-cursors)
(require 'espuds)
(require 'ert)
(require 'wrap-region)

(defun mc/save-lists ()) ;; redefine to do nothing when running tests

(Before
 (cua-mode 0)
 (multiple-cursors-mode 0)
 (rectangular-region-mode 0)
 (global-set-key (kbd "C->") 'mc/mark-next-like-this)
 (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
 (global-set-key (kbd "M-!") 'mc/mark-all-like-this)
 (global-set-key (kbd "M-$") 'mc/mark-all-like-this-dwim)
 (global-set-key (kbd "M-#") 'mc/mark-all-in-region)
 (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
 (global-set-key (kbd "H-SPC") 'set-rectangular-region-anchor)
 (switch-to-buffer
  (get-buffer-create "*multiple-cursors*"))
 (erase-buffer)
 (transient-mark-mode 1)
 (cua-mode 0)
 (delete-selection-mode 0)
 (subword-mode 0)
 (wrap-region-mode 0)
 (setq set-mark-default-inactive nil)
 (deactivate-mark))

(After)
