(And "^delete-selection-mode is active$"
     (lambda ()
       (delete-selection-mode 1)))

(Then "^I should have \\([0-9]+\\) cursors$"
      (lambda (num)
        (let ((actual (mc/num-cursors)))
          (assert (eq (string-to-number num) actual) nil
                  "Expected to have %s cursors, but was %d." num actual))))

(Then "^I should have one cursor$"
      (lambda ()
        (assert (not multiple-cursors-mode) nil
                "Expected to have one cursor, but multiple-cursors-mode is still active.")
        (assert (eq 1 (mc/num-cursors)) nil
                "Expected to have one cursor, but there are still fake cursor overlays.")))

(Then "^rectangular-region-mode should be off$"
       (lambda ()
         (assert (not rectangular-region-mode) nil
                 "Expected rectangular-region-mode mode to be off, but wasn't.")))

(Then "^rectangular-region-mode should be on$"
       (lambda ()
         (assert rectangular-region-mode nil
                 "Expected rectangular-region-mode mode to be on, but wasn't.")))

(When "^I press \"\\(.+\\)\"$"
      (lambda (keybinding)
        (let ((macro (edmacro-parse-keys keybinding)))
          (if espuds-chain-active
              (setq espuds-action-chain (vconcat espuds-action-chain macro))
            (if (and (equal keybinding "C-g")
                     (eq (key-binding (kbd "C-g")) 'keyboard-quit))
                (espuds-quit)
              (execute-kbd-macro macro))))))

(Given "^I have cursors at \"\\(.+\\)\" in \"\\(.+\\)\"$"
       (lambda (needle haystack)
         (insert haystack)
         (goto-char (point-min))
         (search-forward needle)
         (set-mark (point))
         (goto-char (match-beginning 0))
         (mc/mark-all-like-this)
         (mc/keyboard-quit)))

(When "^I copy \"\\(.+\\)\" in another program$"
       (lambda (text)
         (lexical-let ((text text))
           (setq interprogram-paste-function
                 #'(lambda () (let ((r text)) (setq text nil) r))))))

(Given "^I have bound C-! to a lambda that inserts \"\\(.+\\)\"$"
       (lambda (ins)
         (lexical-let ((ins ins))
           (global-set-key (kbd "C-!") #'(lambda () (interactive) (insert ins))))))

(Given "^I have bound C-! to a new command that inserts \"\\(.+\\)\"$"
       (lambda (ins)
         (lexical-let ((ins ins))
           (defun mc-test-temp-command () (interactive) (insert ins))
           (global-set-key (kbd "C-!") 'mc-test-temp-command))))

(Given "^I have bound C-! to another new command that inserts \"\\(.+\\)\"$"
       (lambda (ins)
         (lexical-let ((ins ins))
           (defun mc-test-temp-command-2 () (interactive) (insert ins))
           (global-set-key (kbd "C-!") 'mc-test-temp-command-2))))

(Given "^I have bound C-! to a keyboard macro that insert \"_\"$"
       (lambda ()
         (fset 'mc-test-temp-kmacro "\C-q_")
         (global-set-key (kbd "C-!") 'mc-test-temp-kmacro)))

(When "^I go to character \"\\(.+\\)\"$"
      (lambda (char)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" char) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (assert search nil message char (espuds-buffer-contents)))))

(When "^I go to the \\(front\\|end\\) of the word \"\\(.+\\)\"$"
      (lambda (pos word)
        (goto-char (point-min))
        (let ((search (re-search-forward (format "%s" word) nil t))
              (message "Can not go to character '%s' since it does not exist in the current buffer: %s"))
          (assert search nil message word (espuds-buffer-contents))
          (if (string-equal "front" pos) (backward-word)))))

(When "^I select the last \"\\(.+\\)\"$"
      (lambda (text)
        (goto-char (point-max))
        (let ((search (re-search-backward text nil t)))
          (assert search nil "The text '%s' was not found in the current buffer." text))
        (set-mark (point))
        (re-search-forward text)))

(When "^I mark all \\(.+\\)$"
      (lambda (rest)
        (let ((func (intern (mapconcat 'identity
                                       (cons  "mc/mark-all"
                                              (split-string rest))
                                       "-"))))
          (call-interactively func))))
