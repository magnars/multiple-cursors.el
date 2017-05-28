(let* ((current-directory (file-name-directory load-file-name))
       (features-directory (expand-file-name ".." current-directory))
       (project-directory (expand-file-name ".." features-directory)))
  (setq multiple-cursors-root-path project-directory)
  (setq multiple-cursors-util-path (expand-file-name "util" project-directory)))

(add-to-list 'load-path multiple-cursors-root-path)
(add-to-list 'load-path multiple-cursors-util-path)
(add-to-list 'load-path (expand-file-name "espuds" multiple-cursors-util-path))
(add-to-list 'load-path (expand-file-name "vendor" multiple-cursors-util-path))

(require 'evil)
(require 'evil-surround)
(require 'mc-vars)
(setq mc/list-file "")
;; force the evil specific vars to be added to the cmds to run for all / once
;; will set evil mode to zero in the before hook
;; This is how the loading would work for a user using evil and multiple-cursors together
(evil-mode 1)

(require 'cl-preloaded)
(setf (symbol-function 'cl--assertion-failed)
      (lambda (form &optional string sargs args)
        "Fake version"
        ;; (if debug-on-error
        ;;     (apply debugger `(cl-assertion-failed ,form ,string ,@sargs))
        (if string
            (apply #'error string (append sargs args))
          (signal 'cl-assertion-failed `(,form ,@sargs)))))

(add-to-list 'mc--evil-cmds-to-run-for-all 'evil-surround-region)
(require 'multiple-cursors)
(require 'espuds)
(require 'ert)
(require 'wrap-region)
(require 'evil)

(defun mc/save-lists ()) ;; redefine to do nothing when running tests
(defun mc/prompt-for-inclusion-in-whitelist-test-advice (orig-fun &optional original-command)
  (if (mc/evil-p)
      (error "Command '%s' not included in mc--default-cmds-to-run-for-all or mc--default-cmds-to-run-once" original-command))
  (funcall orig-fun original-command))

(advice-add 'mc/prompt-for-inclusion-in-whitelist :around #'mc/prompt-for-inclusion-in-whitelist-test-advice)

(Before
 (evil-mode 0)
 (evil-surround-mode 1)
 (cua-mode 0)
 (multiple-cursors-mode 0)
 (mc/remove-fake-cursors)
 (setq mc--current-cursor-id 0)
 (rectangular-region-mode 0)
 (global-set-key (kbd "C->") 'mc/mark-next-like-this)
 (global-set-key (kbd "C-S-c C->") 'mc/mark-next-like-this-word)
 (global-set-key (kbd "C-S-c M->") 'mc/mark-next-like-this-symbol)
 (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
 (global-set-key (kbd "C-S-c C-<") 'mc/mark-previous-like-this-word)
 (global-set-key (kbd "C-S-c M-<") 'mc/mark-previous-like-this-symbol)
 (global-set-key (kbd "M-!") 'mc/mark-all-like-this)
 (global-set-key (kbd "M-$") 'mc/mark-all-like-this-dwim)
 (global-set-key (kbd "C-$") 'mc/mark-all-dwim)
 (global-set-key (kbd "M-#") 'mc/mark-all-in-region)
 (global-set-key (kbd "H-0") 'mc/insert-numbers)
 (global-set-key (kbd "H-3") 'mc/insert-letters)
 (global-set-key (kbd "H-1") 'mc/reverse-regions)
 (global-set-key (kbd "H-2") 'mc/sort-regions)
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
